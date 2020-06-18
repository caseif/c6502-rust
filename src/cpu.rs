use crate::instrs::*;

const DEFAULT_STATUS: u8 = 0x24;

type StatusReg = u8;

#[derive(Copy, Clone)]
enum StatusBit {
    Carry = 0x7,
    Zero = 0x6,
    InterruptDisable = 0x5,
    Decimal = 0x4,
    BreakCommand = 0x3,
    Overflow = 0x1,
    Negative = 0x0
}

impl Into<u8> for StatusBit {
    fn into(self) -> u8 {
        self as u8
    }
}

trait StatusBitfield {
    fn get_bit(&self, bit: StatusBit) -> bool;
    fn set_bit(self, bit: StatusBit, val: bool) -> ();
}

impl StatusBitfield for StatusReg {
    fn get_bit(&self, bit: StatusBit) -> bool {
        assert!((0..7).contains(&(bit as u8)));
        ((self >> (bit as u8)) & 1) == 1
    }
    
    fn set_bit(mut self, bit: StatusBit, val: bool) -> () {
        assert!((0..7).contains(&(bit as u8)));
        if val {
            self |= 1 << (bit as u8);
        } else {
            self &= !(1 << (bit as u8));
        }
    }
}

#[derive(Clone, Copy, Default)]
struct CpuRegs {
    status: StatusReg,
    pc: u16,
    sp: u8,
    acc: u8,
    x: u8,
    y: u8
}

struct InterruptProps {
    vector: u16,
    maskable: bool,
    push_pc: bool,
    set_b: bool,
    set_i: bool
}

enum InterruptType {
    NMI,
    RST,
    IRQ,
    BRK
}

impl InterruptType {
    fn props(&self) -> InterruptProps {
        match self {
            NMI => InterruptProps { vector: 0xFFFA, maskable: false, push_pc: true, set_b: false, set_i: false },
            RST => InterruptProps { vector: 0xFFFC, maskable: false, push_pc: false,  set_b: false, set_i: true },
            IRQ => InterruptProps { vector: 0xFFFE, maskable: true,  push_pc: true,  set_b: false, set_i: true },
            BRK => InterruptProps { vector: 0xFFFE, maskable: false, push_pc: true,  set_b: true,  set_i: true }
        }
    }

    pub fn vector(&self) -> u16     { self.props().vector }
    pub fn maskable(&self) -> bool  { self.props().maskable }
    pub fn push_pc(&self) -> bool   { self.props().push_pc }
    pub fn set_b(&self) -> bool     { self.props().set_b }
    pub fn set_i(&self) -> bool     { self.props().set_i }
}

#[derive(Clone, Copy)]
pub struct CpuSysIface {
    mem_read:       fn(u16) -> u8,
    mem_write:      fn(u16, u8) -> (),
    bus_read:       fn() -> u8,
    bus_write:      fn(u8) -> (),
    poll_nmi_line:  fn() -> bool,
    poll_irq_line:  fn() -> bool,
    poll_rst_line:  fn() -> bool,
}

impl Default for CpuSysIface {
    fn default() -> CpuSysIface {
        CpuSysIface {
            mem_read: |_addr| 0,
            mem_write: |_addr, _val| (),
            bus_read: || 0,
            bus_write: |_val| (),
            poll_nmi_line: || true,
            poll_irq_line: || true,
            poll_rst_line: || true,
        }
    }
}

#[derive(Default)]
struct Cpu {
    sys_iface: CpuSysIface,
    regs: CpuRegs,
    nmi_edge_det: bool,
    irq_line_rdr: bool,
    rst_line_rdr: bool,
    nmi_line_last: bool,
    // state for implementing cycle-accuracy
    instr_cycle: u8, // this is 1-indexed to match blargg's doc
    cur_instr: Option<&'static Instr>,
    last_opcode: u8,
    cur_operand: u16,
    eff_operand: u16,
    cur_int: Option<InterruptType>,
    queued_int: Option<InterruptType>,
    nmi_hijack: bool,
    regs_snapshot: CpuRegs
}

static mut SYS_IFACE: Option<CpuSysIface> = None;

fn sys_iface() -> &'static CpuSysIface {
    unsafe {
        return SYS_IFACE.as_ref().unwrap();
    }
}

impl Cpu {
    pub fn new(sys_iface: CpuSysIface) -> Cpu {
        let mut cpu: Cpu = Default::default();

        cpu.regs.status = DEFAULT_STATUS;
        cpu.regs_snapshot = cpu.regs;
        
        cpu.queued_int = Some(InterruptType::RST);

        return cpu;
    }

    fn next_prg_byte(&self) -> u8 {
        return (self.sys_iface.mem_read)(self.regs.pc);
    }

    fn set_alu_flags(&self, val: u8) -> () {
        self.regs.status.set_bit(StatusBit::Zero, val == 0);
        self.regs.status.set_bit(StatusBit::Negative, (val & 0x80) != 0);
    }

    fn do_shift(&self, right: bool, rot: bool) {
        let mut res: u8 = (self.sys_iface.bus_read)();
        if right {
            res >>= 1;
            if rot {
                res |= (self.regs.status.get_bit(StatusBit::Carry) as u8) << 7;
            }

            self.regs.status.set_bit(StatusBit::Carry, (self.sys_iface.bus_read)() != 0);
        } else {
            res <<= 1;
            if rot {
                res |= self.regs.status.get_bit(StatusBit::Carry) as u8;
            }
            self.regs.status.set_bit(StatusBit::Carry, ((self.sys_iface.bus_read)() & 0x80) >> 7 != 0);
        }

        self.set_alu_flags(res);

        (self.sys_iface.bus_write)(res);
    }

    fn do_cmp(&self, reg: u8, m: u8) -> () {
        self.regs.status.set_bit(StatusBit::Carry, reg >= m);
        self.regs.status.set_bit(StatusBit::Zero, reg == m);
        self.regs.status.set_bit(StatusBit::Negative, ((reg - m) >> 7) == 1);
    }

    fn do_adc(&mut self, m: u8) -> () {
        let acc0: u8 = self.regs.acc;

        self.regs.acc = acc0 + m + self.regs.status.get_bit(StatusBit::Carry) as u8;

        self.set_alu_flags(self.regs.acc);

        // unsigned overflow will occur if at least two among the most
        // significant operand bits and the carry bit are set
        self.regs.status.set_bit(StatusBit::Carry, (((acc0 + m + self.regs.status.get_bit(StatusBit::Carry) as u8) as u16) & 0x100) != 0);
        self.regs.status.set_bit(StatusBit::Overflow, ((acc0 ^ self.regs.acc) & (m ^ self.regs.acc) & 0x80) != 0);
    }

    fn do_sbc(&mut self, m: u8) -> () {
        self.do_adc(!m)
    }

    fn do_instr_operation(&mut self) {
        assert!(self.cur_instr.is_some());

        use Mnemonic::*;
        match self.cur_instr.unwrap().mnemonic {
            LDA => {
                self.regs.acc = (self.sys_iface.bus_read)();
                self.set_alu_flags(self.regs.acc);
            },
            LDX => {
                self.regs.x = (self.sys_iface.bus_read)();
                self.set_alu_flags(self.regs.x);
            },
            LDY => {
                self.regs.y = (self.sys_iface.bus_read)();
                self.set_alu_flags(self.regs.y);
            },
            _ => {}
        }
    }
    
}
