use crate::instrs::*;

const STACK_BOTTOM_ADDR: u16 = 0x100;
const DEFAULT_STATUS: u8 = 0x24;

pub type StatusReg = u8;

#[derive(Copy, Clone)]
pub enum StatusBit {
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

pub trait StatusBitfield {
    fn get_bit(&self, bit: StatusBit) -> bool;
    fn set_bit(&mut self, bit: StatusBit, val: bool) -> ();
}

impl StatusBitfield for StatusReg {
    fn get_bit(&self, bit: StatusBit) -> bool {
        assert!((0..=7).contains(&(bit as u8)));
        ((self >> (bit as u8)) & 1) == 1
    }
    
    fn set_bit(&mut self, bit: StatusBit, val: bool) -> () {
        assert!((0..=7).contains(&(bit as u8)));
        if val {
            *self |= 1 << (bit as u8);
        } else {
            *self &= !(1 << (bit as u8));
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
    push_pc: bool,
    set_b: bool,
    set_i: bool
}

#[derive(PartialEq)]
enum InterruptType {
    NMI,
    RST,
    IRQ,
    BRK
}

impl InterruptType {
    fn props(&self) -> InterruptProps {
        use InterruptType::*;
        match self {
            NMI => InterruptProps { vector: 0xFFFA, push_pc: true,  set_b: false, set_i: false },
            RST => InterruptProps { vector: 0xFFFC, push_pc: false, set_b: false, set_i: true },
            IRQ => InterruptProps { vector: 0xFFFE, push_pc: true,  set_b: false, set_i: true },
            BRK => InterruptProps { vector: 0xFFFE, push_pc: true,  set_b: true,  set_i: true }
        }
    }

    pub fn vector(&self) -> u16     { self.props().vector }
    pub fn push_pc(&self) -> bool   { self.props().push_pc }
    pub fn set_b(&self) -> bool     { self.props().set_b }
    pub fn set_i(&self) -> bool     { self.props().set_i }
}

pub trait SysMemIface {
    fn read(&mut self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8) -> ();
}

pub trait SysBusIface {
    fn read(&self) -> u8;
    fn write(&mut self, val: u8) -> ();
}

pub trait SysIntLinesIface {
    fn poll_nmi(&self) -> bool;
    fn poll_irq(&self) -> bool;
    fn poll_rst(&self) -> bool;
}

pub struct Cpu {
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
    cur_int: Option<&'static InterruptType>,
    queued_int: Option<&'static InterruptType>,
    nmi_hijack: bool,
    regs_snapshot: CpuRegs,
    log_callback: Option<fn (&str) -> ()>
}

impl Default for Cpu {
    fn default() -> Cpu {
        Cpu {
            regs: Default::default(),
            nmi_edge_det: Default::default(),
            irq_line_rdr: Default::default(),
            rst_line_rdr: Default::default(),
            nmi_line_last: Default::default(),
            instr_cycle: 1,  // this is 1-indexed to match blargg's doc
            cur_instr: Default::default(),
            last_opcode: Default::default(),
            cur_operand: Default::default(),
            eff_operand: Default::default(),
            cur_int: Default::default(),
            queued_int: Default::default(),
            nmi_hijack: Default::default(),
            regs_snapshot: Default::default(),
            log_callback: Default::default(),
        }
    }
}

impl Cpu {
    pub fn new() -> Cpu {
        let mut cpu: Cpu = Default::default();

        cpu.regs.status = DEFAULT_STATUS;
        cpu.regs_snapshot = cpu.regs;
        
        cpu.queued_int = Some(&InterruptType::RST);

        return cpu;
    }

    pub fn set_log_callback(&mut self, callback: Option<fn (&str) -> ()>) -> () {
        self.log_callback = callback;
    }

    // core logic

    fn set_alu_flags(&mut self, val: u8) -> () {
        self.regs.status.set_bit(StatusBit::Zero, val == 0);
        self.regs.status.set_bit(StatusBit::Negative, (val & 0x80) != 0);
    }

    fn do_shift(&mut self, bus: &mut impl SysBusIface, right: bool, rot: bool) {
        let mut res: u8 = bus.read();
        if right {
            res >>= 1;
            
            if rot {
                res |= (self.regs.status.get_bit(StatusBit::Carry) as u8) << 7;
            }

            self.regs.status.set_bit(StatusBit::Carry, bus.read() & 0x01 != 0);
        } else {
            res <<= 1;
            
            if rot {
                res |= self.regs.status.get_bit(StatusBit::Carry) as u8;
            }
            
            self.regs.status.set_bit(StatusBit::Carry, (bus.read() & 0x80) >> 7 != 0);
        }

        self.set_alu_flags(res);

        bus.write(res);
    }

    fn do_cmp(&mut self, reg: u8, m: u8) -> () {
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
        self.regs.status.set_bit(StatusBit::Carry, ((acc0 as u16 + m as u16 + self.regs.status.get_bit(StatusBit::Carry) as u16) & 0x100) != 0);
        // signed overflow will occur if the sign of both inputs is
        // different from the sign of the result
        self.regs.status.set_bit(StatusBit::Overflow, ((acc0 ^ self.regs.acc) & (m ^ self.regs.acc) & 0x80) != 0);
    }

    fn do_sbc(&mut self, m: u8) -> () {
        self.do_adc(!m)
    }

    fn do_instr_operation(&mut self, bus: &mut impl SysBusIface) {
        assert!(self.cur_instr.is_some());

        use Mnemonic::*;
        match self.cur_instr.unwrap().mnemonic {
            LDA => {
                self.regs.acc = bus.read();
                self.set_alu_flags(self.regs.acc);
            }
            LDX => {
                self.regs.x = bus.read();
                self.set_alu_flags(self.regs.x);
            }
            LDY => {
                self.regs.y = bus.read();
                self.set_alu_flags(self.regs.y);
            }
            LAX => { // unofficial
                self.regs.acc = bus.read();
                self.regs.x = bus.read();

                self.set_alu_flags(bus.read());
            },
            STA => {
                bus.write(self.regs.acc);
            }
            STX => {
                bus.write(self.regs.x);
            }
            STY => {
                bus.write(self.regs.y);
            }
            TAX => {
                self.regs.x = self.regs.acc;

                self.set_alu_flags(self.regs.x);
            }
            TAY => {
                self.regs.y = self.regs.acc;

                self.set_alu_flags(self.regs.y);
            }
            TSX => {
                self.regs.x = self.regs.sp;

                self.set_alu_flags(self.regs.x);
            }
            TXA => {
                self.regs.acc = self.regs.x;

                self.set_alu_flags(self.regs.acc);
            }
            TYA => {
                self.regs.acc = self.regs.y;

                self.set_alu_flags(self.regs.acc);
            }
            TXS => {
                self.regs.sp = self.regs.x;
            }
            // math
            ADC => {
                self.do_adc(bus.read());
            }
            SBC => {
                self.do_sbc(bus.read());
            }
            DEC => {
                bus.write(bus.read() - 1);

                self.set_alu_flags(bus.read());
            }
            DEX => {
                self.regs.x -= 1;

                self.set_alu_flags(self.regs.x);
            }
            DEY => {
                self.regs.y -= 1;

                self.set_alu_flags(self.regs.y);
            }
            INC => {
                bus.write(bus.read() + 1);

                self.set_alu_flags(bus.read());
            }
            INX => {
                self.regs.x += 1;

                self.set_alu_flags(self.regs.x);
            }
            INY => {
                self.regs.y += 1;

                self.set_alu_flags(self.regs.y);
            }
            ISC => { // unofficial
                bus.write(bus.read() + 1);
                self.do_sbc(bus.read());
            }
            DCP => { // unofficial
                bus.write(bus.read() - 1);
                self.do_cmp(self.regs.acc, bus.read());
            }
            // logic
            AND => {
                self.regs.acc &= bus.read();

                self.set_alu_flags(self.regs.acc);
            }
            SAX => { // unofficial
                let res: u8 = self.regs.acc & self.regs.x;
                bus.write(res);
            }
            ANC => { // unofficial
                self.regs.acc &= bus.read();
                self.regs.status.set_bit(StatusBit::Carry, (self.regs.acc >> 7) != 0);
            }
            ASL => {
                self.do_shift(bus, false, false);
            }
            LSR => {
                self.do_shift(bus, true, false);
            }
            ROL => {
                self.do_shift(bus, false, true);
            }
            ROR => {
                self.do_shift(bus, true, true);
            }
            ALR => { // unofficial
                self.do_shift(bus, true, false);
            }
            SLO => { // unofficial
                self.do_shift(bus, false, false);
                self.regs.acc |= bus.read();

                self.set_alu_flags(self.regs.acc);
            }
            RLA => { // unofficial
                // I think this performs two r/w cycles too
                self.do_shift(bus, false, true);

                self.regs.acc &= bus.read();

                self.set_alu_flags(self.regs.acc);
            }
            ARR => { // unofficial
                self.regs.acc &= bus.read();

                self.do_shift(bus, true, true);

                self.set_alu_flags(self.regs.acc);

                self.regs.status.set_bit(StatusBit::Overflow, ((self.regs.acc >> 5) & 1) != 0);
                self.regs.status.set_bit(StatusBit::Carry, (!((self.regs.acc >> 6) & 1)) != 0);
            }
            SRE => { // unofficial
                self.do_shift(bus, true, false);

                self.regs.acc ^= bus.read();

                self.set_alu_flags(self.regs.acc);
            }
            RRA => { // unofficial
                self.do_shift(bus, true, true);

                self.do_adc(bus.read());
            }
            AXS => { // unofficial
                self.regs.x &= self.regs.acc;
                let res: u8 = self.regs.x - bus.read();

                self.regs.status.set_bit(StatusBit::Carry, res > bus.read());

                self.regs.x = res;

                self.set_alu_flags(self.regs.x);
            }
            EOR => {
                self.regs.acc = self.regs.acc ^ bus.read();

                self.set_alu_flags(self.regs.acc);
            }
            ORA => {
                self.regs.acc = self.regs.acc | bus.read();

                self.set_alu_flags(self.regs.acc);
            }
            BIT => {
                // set negative and overflow flags from memory
                self.regs.status.set_bit(StatusBit::Negative, (bus.read() >> 7) != 0);
                self.regs.status.set_bit(StatusBit::Overflow, ((bus.read() >> 6) & 1) != 0);

                // mask accumulator with value and set zero flag appropriately
                self.regs.status.set_bit(StatusBit::Zero, (self.regs.acc & bus.read()) == 0);
            }
            TAS => { // unofficial
                // this some fkn voodo right here
                self.regs.sp = self.regs.acc & self.regs.x;
                bus.write(self.regs.sp & (((self.cur_operand >> 8) as u8) + 1));
            }
            LAS => { // unofficial
                self.regs.acc = bus.read() & self.regs.sp;
                self.regs.x = self.regs.acc;
                self.regs.sp = self.regs.acc;

                self.set_alu_flags(self.regs.acc);
            }
            XAS => { // unofficial
                //TODO: this instruction is supposed to take 5 cycles; currently it takes 7
                bus.write(self.regs.x & (((self.cur_operand >> 8) as u8) + 1));
            }
            SAY => { // unofficial
                //TODO: same deal as XAS
                bus.write(self.regs.y & (((self.cur_operand >> 8) as u8) + 1));
            }
            AXA => { // unofficial
                //TODO: same deal as AXA, except it has two addressing modes
                bus.write((self.regs.acc & self.regs.x) & 7);
            }
            XAA => { // unofficial
                // even more voodoo
                self.regs.acc = (self.regs.x & 0xEE) | ((self.regs.x & self.regs.acc) & 0x11);
            }
            // registers
            CLC => {
                self.regs.status.set_bit(StatusBit::Carry, false);
            }
            CLD => {
                self.regs.status.set_bit(StatusBit::Decimal, false);
            }
            CLI => {
                self.regs.status.set_bit(StatusBit::InterruptDisable, false);
            }
            CLV => {
                self.regs.status.set_bit(StatusBit::Overflow, false);
            }
            CMP => {
                self.do_cmp(self.regs.acc, bus.read());
            }
            CPX => {
                self.do_cmp(self.regs.x, bus.read());
            }
            CPY => {
                self.do_cmp(self.regs.y, bus.read());
            }
            SEC => {
                self.regs.status.set_bit(StatusBit::Carry, true);
            }
            SED => {
                self.regs.status.set_bit(StatusBit::Decimal, true);
            }
            SEI => {
                self.regs.status.set_bit(StatusBit::InterruptDisable, true);
            }
            // misc
            NOP => {
                // no-op
            }
            KIL => {
                panic!("Encountered KIL instruction");
            }
            _ => panic!("Encountered unexpected mnemonic {:?}", self.cur_instr.unwrap().mnemonic)
        }
    }

    // helper functions

    fn assert_cycle(&self, min: u8, max: u8) {
        assert!(self.instr_cycle >= min && self.instr_cycle <= max, "Unexpected cycle {} (expected range [{}, {}])",
            self.instr_cycle, min, max);
    }

    fn stack_read(&self, mem: &mut impl SysMemIface) -> u8 {
        mem.read(STACK_BOTTOM_ADDR + self.regs.sp as u16)
    }

    fn stack_write(&mut self, mem: &mut impl SysMemIface, val: u8) -> () {
        mem.write(STACK_BOTTOM_ADDR + self.regs.sp as u16, val);
    }

    // interrupt logic

    fn read_interrupt_lines(&mut self, int_lines: &impl SysIntLinesIface) -> () {
        self.nmi_edge_det |= self.nmi_line_last && !int_lines.poll_nmi();

        self.nmi_line_last = int_lines.poll_nmi();

        self.irq_line_rdr = !int_lines.poll_irq();
        self.rst_line_rdr = !int_lines.poll_rst();
    }

    fn poll_interrupts(&mut self) -> () {
        if self.nmi_edge_det {
            self.queued_int = Some(&InterruptType::NMI);
        } else if self.irq_line_rdr && !self.regs.status.get_bit(StatusBit::InterruptDisable) {
            self.queued_int = Some(&InterruptType::IRQ);
        } else if self.rst_line_rdr {
            self.queued_int = Some(&InterruptType::RST);
        }
    }

    fn exec_interrupt(&mut self, mem: &mut impl SysMemIface) -> () {
        assert!(self.cur_int.is_some());
        self.assert_cycle(1, 7);

        if self.instr_cycle <= 4 && self.cur_int.unwrap() == &InterruptType::BRK && self.nmi_edge_det {
            self.nmi_hijack = true;
        }

        match self.instr_cycle {
            1 => {
                self.next_prg_byte(mem); // garbage read
                self.last_opcode = 0; // BRK
            }
            2 => {
                self.next_prg_byte(mem); // garbage read

                if self.cur_int.unwrap() == &InterruptType::BRK {
                    self.regs.pc += 1; // increment PC anyway for software interrupts
                }
            }
            3 => {
                if self.cur_int.unwrap().push_pc() {
                    // push PC high
                    self.stack_write(mem, (self.regs.pc >> 8) as u8);
                }

                // decrement S
                self.regs.sp -= 1;

                if self.cur_int.unwrap() == &InterruptType::BRK && self.nmi_edge_det {
                    self.nmi_hijack = true;
                }
            }
            4 => {
                if self.cur_int.unwrap().push_pc() {
                    // push PC low
                    self.stack_write(mem, (self.regs.pc & 0xFF) as u8);
                }

                // decrement S
                self.regs.sp -= 1;

                if self.cur_int.unwrap() == &InterruptType::BRK && self.nmi_edge_det {
                    self.nmi_hijack = true;
                }
            }
            5 => {
                if self.nmi_hijack {
                    self.cur_int = Some(&InterruptType::NMI);
                    self.nmi_hijack = false;
                }

                if self.cur_int.unwrap().push_pc() {
                    // push P
                    let mut p = self.regs.status;
                    if self.cur_int.unwrap() == &InterruptType::BRK {
                        p |= 0x30;
                    }
                    self.stack_write(mem, p);

                    // set/clear B
                    self.regs.status.set_bit(StatusBit::BreakCommand, self.cur_int.unwrap().set_b());
                }

                // decrement S
                self.regs.sp -= 1;
            }
            6 => {
                // clear PC low and set to vector value
                self.regs.pc &= 0xFF00;
                self.regs.pc |= mem.read(self.cur_int.unwrap().vector()) as u16;

                if self.cur_int.unwrap().set_i() {
                    self.regs.status.set_bit(StatusBit::InterruptDisable, true);
                }
            }
            7 => {
                // clear PC high and set to vector value
                self.regs.pc &= 0xFF;
                self.regs.pc |= (mem.read(self.cur_int.unwrap().vector() + 1) as u16) << 8;
                self.instr_cycle = 0; // reset for next instruction

                match self.cur_int.unwrap() {
                    &InterruptType::NMI => self.nmi_edge_det = false,
                    &InterruptType::IRQ => self.irq_line_rdr = false,
                    &InterruptType::BRK | &InterruptType::RST => self.rst_line_rdr = false
                };

                self.cur_int = None;

                // update the register snapshot to reflect state after the interrupt
                self.regs_snapshot = self.regs;
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        };
    }

    // special instruction logic

    fn handle_rti(&mut self, mem: &mut impl SysMemIface) -> () {
        self.assert_cycle(2, 6);

        match self.instr_cycle {
            2 => {
                self.next_prg_byte(mem); // garbage read
            }
            3 => {
                // increment S
                self.regs.sp += 1;
            }
            4 => {
                // pull P, increment S
                self.regs.status = self.stack_read(mem);
                self.regs.sp += 1;
            }
            5 => {
                // clear PC low and set to stack value, increment S
                self.regs.pc &= !0xFFu16;
                self.regs.pc |= self.stack_read(mem) as u16;
                self.regs.sp += 1;
            }
            6 => {
                // clear PC high and set to stack value
                self.regs.pc &= 0xFF;
                self.regs.pc |= (self.stack_read(mem) as u16) << 8;
                self.instr_cycle = 0; // reset for next instruction
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    fn handle_rts(&mut self, mem: &mut impl SysMemIface) -> () {
        self.assert_cycle(2, 6);
        
        match self.instr_cycle {
            2 => {
                mem.read(self.regs.pc); // garbage read
            }
            3 => {
                // increment S
                self.regs.sp += 1;
            }
            4 => {
                // clear PC low and set to stack value, increment S
                self.regs.pc &= !0xFFu16;
                self.regs.pc |= self.stack_read(mem) as u16;
                self.regs.sp += 1;
            }
            5 => {
                // clear PC high and set to stack value
                self.regs.pc &= 0xFF;
                self.regs.pc |= (self.stack_read(mem) as u16) << 8;
            }
            6 => {
                // increment PC
                self.regs.pc += 1;
                self.instr_cycle = 0; // reset for next instructino
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    fn handle_jsr(&mut self, mem: &mut impl SysMemIface) -> () {
        self.assert_cycle(3, 6);

        match self.instr_cycle {
            3 => {
                // unsure of what happens here
            }
            4 => {
                // push PC high, decrement S
                self.stack_write(mem, (self.regs.pc >> 8) as u8);
                self.regs.sp -= 1;
            }
            5 => {
                // push PC low, decrement S
                self.stack_write(mem, (self.regs.pc & 0xFF) as u8);
                self.regs.sp -= 1;
            }
            6 => {
                // copy low byte to PC, fetch high byte to PC (but don't increment PC)
                self.cur_operand |= (mem.read(self.regs.pc) as u16) << 8;
                self.eff_operand = self.cur_operand;
                self.regs.pc = self.cur_operand;

                self.instr_cycle = 0; // reset for next instruction
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    fn handle_stack_push(&mut self, mem: &mut impl SysMemIface) -> () {
        self.assert_cycle(2, 3);

        match self.instr_cycle {
            2 => {
                self.next_prg_byte(mem); // garbage read
            }
            3 => {
                // push register, decrement S
                self.stack_write(mem, if self.cur_instr.unwrap().mnemonic == Mnemonic::PHA {
                    self.regs.acc
                } else {
                    self.regs.status | 0x30
                });

                self.regs.sp -= 1;

                self.instr_cycle = 0; // reset for next instruction
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    fn handle_stack_pull(&mut self, mem: &mut impl SysMemIface) -> () {
        self.assert_cycle(2, 4);

        match self.instr_cycle {
            2 => {
                self.next_prg_byte(mem); // garbage read
            }
            3 => {
                // increment S
                self.regs.sp += 1;
            }
            4 => {
                // pull register
                if self.cur_instr.unwrap().mnemonic == Mnemonic::PLA {
                    self.regs.acc = self.stack_read(mem);
                    self.set_alu_flags(self.regs.acc);
                } else {
                    self.regs.status = self.stack_read(mem);
                }

                self.instr_cycle = 0; // reset for next instruction
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    fn handle_stack_instr(&mut self, mem: &mut impl SysMemIface) -> () {
        use Mnemonic::*;
        match self.cur_instr.unwrap().mnemonic {
            PHA | PHP => {
                self.handle_stack_push(mem);
            }
            PLA | PLP => {
                self.handle_stack_pull(mem);
            }
            _ => panic!("Unexpected instruction mnemonic {:?}", self.cur_instr.unwrap().mnemonic)
        }
    }

    // general instruction logic

    fn handle_instr_rw(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface, offset: u8) -> () {
        use InstrType::*;
        match self.cur_instr.unwrap().mnemonic.get_type() {
            Read => {
                self.assert_cycle(offset, offset);

                bus.write(mem.read(self.eff_operand));
                self.do_instr_operation(bus);

                self.instr_cycle = 0;
            }
            Write => {
                self.assert_cycle(offset, offset);

                self.do_instr_operation(bus);
                mem.write(self.eff_operand, bus.read());

                self.instr_cycle = 0;
            }
            ReadWrite => {
                self.assert_cycle(offset, offset + 2);

                match self.instr_cycle - offset {
                    0 => {
                        bus.write(mem.read(self.eff_operand));
                    }
                    1 => {
                        mem.write(self.eff_operand, bus.read());
                        self.do_instr_operation(bus);
                    }
                    2 => {
                        mem.write(self.eff_operand, bus.read());
                        self.instr_cycle = 0;
                    }
                    _ => panic!("(cycle - offset) not within [0,2]!")
                }
            }
            _ => panic!("Unexpected instruction {:?}", self.cur_instr.unwrap().mnemonic)
        }
    }

    fn handle_instr_zrp(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface) -> () {
        self.eff_operand = self.cur_operand;
        self.handle_instr_rw(mem, bus, 3);
    }

    fn handle_instr_zpi(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface) -> () {
        self.assert_cycle(3, 6);

        if self.instr_cycle == 3 {
            bus.write(mem.read(self.cur_operand));
            let reg_val = if self.cur_instr.unwrap().addr_mode == AddrMode::ZPX {
                self.regs.x
            } else {
                self.regs.y
            };
            self.eff_operand = (self.cur_operand + reg_val as u16) & 0xFF;
        } else {
            self.handle_instr_rw(mem, bus, 4);
        }
    }

    fn handle_instr_abs(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface) -> () {
        self.assert_cycle(3, 6);

        if self.instr_cycle == 3 {
            self.cur_operand |= (self.next_prg_byte(mem) as u16) << 8;
            self.regs.pc += 1;
        } else {
            self.eff_operand = self.cur_operand;
            self.handle_instr_rw(mem, bus, 4);
        }
    }
    
    fn handle_instr_abi(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface) -> () {
        self.assert_cycle(3, 8);

        match self.instr_cycle {
            3 => {
                self.cur_operand |= (self.next_prg_byte(mem) as u16) << 8; // fetch high byte of operand

                let reg_val = if self.cur_instr.unwrap().addr_mode == AddrMode::ABX {
                    self.regs.x
                } else {
                    self.regs.y
                };
                self.eff_operand = (self.cur_operand & 0xFF00) | ((self.cur_operand + reg_val as u16) & 0xFF);

                self.regs.pc += 1; // increment PC
            }
            4 => {
                bus.write(mem.read(self.eff_operand));
                
                // fix effective address
                let reg_val = if self.cur_instr.unwrap().addr_mode == AddrMode::ABX {
                    self.regs.x
                } else {
                    self.regs.y
                };
                if ((self.cur_operand & 0xFF) + reg_val as u16) > 0x100 {
                    self.eff_operand += 0x100;
                } else if self.cur_instr.unwrap().mnemonic.get_type() == InstrType::Read {
                    // we're finished here if the high byte was correect
                    self.do_instr_operation(bus);

                    self.instr_cycle = 0;
                }
            }
            5..=8 => {
                self.handle_instr_rw(mem, bus, 5);
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    fn handle_instr_izx(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface) -> () {
        self.assert_cycle(3, 8);

        match self.instr_cycle {
            3 => {
                mem.read(self.cur_operand); // garbage read
                self.cur_operand = (self.cur_operand & 0xFF00) | ((self.cur_operand + self.regs.x as u16) & 0xFF);
            }
            4 => {
                self.eff_operand = mem.read(self.cur_operand) as u16;
            }
            5 => {
                self.eff_operand |= (mem.read(
                    (self.cur_operand & 0xFF00) | ((self.cur_operand + 1) & 0xFF)
                ) as u16) << 8;
            }
            6 => {
                self.handle_instr_rw(mem, bus, 6);
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    fn handle_instr_izy(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface) -> () {
        self.assert_cycle(3, 8);

        match self.instr_cycle {
            3 => {
                self.eff_operand &= 0xFF00;
                self.eff_operand |= mem.read(self.cur_operand) as u16;
            }
            4 => {
                self.eff_operand &= 0xFF;
                self.eff_operand |= (mem.read(
                    (self.cur_operand & 0xFF00) | ((self.cur_operand + 1) & 0xFF)
                ) as u16) << 8;

                self.eff_operand = (self.eff_operand & 0xFF00) | ((self.eff_operand + self.regs.y as u16) & 0xFF);
            }
            5 => {
                mem.read(self.eff_operand); // garbage read

                // need to correct the high byte
                if self.regs.y > (self.eff_operand & 0xFF) as u8 {
                    self.eff_operand += 0x100;
                    // need to deal with instr operation on next cycle
                } else if self.cur_instr.unwrap().mnemonic.get_type() == InstrType::Read {
                    // we're finished if the high byte was correct, correct value is on bus

                    self.do_instr_operation(bus);

                    self.instr_cycle = 0;
                }
            }
            6 => {
                self.handle_instr_rw(mem, bus, 6);
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    fn handle_jmp(&mut self, mem: &mut impl SysMemIface) -> () {
        match self.cur_instr.unwrap().addr_mode {
            AddrMode::ABS => {
                self.assert_cycle(3, 3);

                let pch = mem.read(self.regs.pc);
                self.regs.pc += 1;

                self.cur_operand |= (pch as u16) << 8;

                self.regs.pc = self.cur_operand;

                self.instr_cycle = 0;
            }
            AddrMode::IND => {
                self.assert_cycle(3, 5);

                match self.instr_cycle {
                    3 => {
                        self.cur_operand |= (self.next_prg_byte(mem) as u16) << 8; // fetch high byte of operand
                        self.regs.pc += 1; // increment PC
                    }
                    4 => {
                        self.eff_operand &= 0xFF00;
                        self.eff_operand |= mem.read(self.cur_operand) as u16; // fetch target low
                    }
                    5 => {
                        self.regs.pc = 0; // clear PC (technically not accurate, but it has no practical consequence)
                        // fetch target high to PC
                        // we technically don't do this properly, but sub-cycle accuracy is not necessarily a goal
                        // page boundary crossing is not handled correctly on real 6502 - we emulate this bug here
                        self.regs.pc |= (mem.read((self.cur_operand & 0xFF00) | ((self.cur_operand + 1) & 0xFF)) as u16) << 8;
                        // copy low address byte to PC
                        self.regs.pc |= self.eff_operand & 0xFF;
                        
                        // this is for logging purposes only
                        self.eff_operand = self.regs.pc;

                        self.instr_cycle = 0;
                    }
                    _ => panic!("Unexpected cycle number {}", self.instr_cycle)
                }
            }
            _ => panic!("Unexpected addressing mode {:?}", self.cur_instr.unwrap().addr_mode)
        }
    }

    fn handle_branch(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface) -> () {
        self.assert_cycle(3, 4);

        match self.instr_cycle {
            3 => {
                bus.write(mem.read(self.regs.pc));

                self.eff_operand = self.regs.pc + (self.cur_operand & 0xFF);

                use Mnemonic::*;
                if match self.cur_instr.unwrap().mnemonic {
                    BCC => !self.regs.status.get_bit(StatusBit::Carry),
                    BCS => self.regs.status.get_bit(StatusBit::Carry),
                    BNE => !self.regs.status.get_bit(StatusBit::Zero),
                    BEQ => self.regs.status.get_bit(StatusBit::Zero),
                    BPL => !self.regs.status.get_bit(StatusBit::Negative),
                    BMI => self.regs.status.get_bit(StatusBit::Negative),
                    BVC => !self.regs.status.get_bit(StatusBit::Overflow),
                    BVS => self.regs.status.get_bit(StatusBit::Overflow),
                    _ => panic!("Unexpected instruction mnemonic {:?}", self.cur_instr.unwrap().mnemonic)
                } {
                    bus.write((self.regs.pc & 0xFF) as u8);
                    self.regs.pc = (self.regs.pc & 0xFF00) | ((self.regs.pc + self.cur_operand as u16) & 0xFF);
                } else {
                    // (indirectly) recursive call to fetch the next opcode
                    self.instr_cycle = 1;
                    self.do_instr_cycle(mem, bus);
                }
            }
            4 => {
                self.poll_interrupts();

                let old_pcl = bus.read();

                bus.write(mem.read(self.regs.pc));

                let neg_offset_mag = !self.cur_operand as u8 + 1u8;
                if self.cur_operand & 0x80 != 0 && neg_offset_mag > old_pcl {
                    self.regs.pc -= 0x100;
                } else if self.cur_operand & 0x80 == 0 && (self.cur_operand & 0xFF) + old_pcl as u16 >= 0x100 {
                    self.regs.pc += 0x100;
                } else {
                    // (indirectly) recursive call to fetch the next opcode
                    self.instr_cycle = 1;
                    self.do_instr_cycle(mem, bus);
                    return;
                }

                self.instr_cycle = 0;
            }
            _ => panic!("Unexpected cycle number {}", self.instr_cycle)
        }
    }

    // execution flow logic

    fn next_prg_byte(&self, mem: &mut impl SysMemIface) -> u8 {
        return mem.read(self.regs.pc);
    }

    fn reset_instr_state(&mut self, bus: &mut impl SysBusIface) -> () {
        self.cur_operand = 0; // reset current operand
        self.eff_operand = 0; // reset effective operand
        bus.write(0); // reset data value
        self.instr_cycle = 1; // skip opcode fetching
    }

    fn do_instr_cycle(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface) -> () {
        if self.cur_int.is_some() {
            self.exec_interrupt(mem);
        } else if self.instr_cycle == 1 {
            self.print_cur_instr(bus);

            if self.queued_int.is_some() {
                self.cur_instr = None;
                self.cur_int = self.queued_int;
                self.queued_int = None;
                self.exec_interrupt(mem);
            } else {
                self.last_opcode = self.next_prg_byte(mem);
                self.cur_instr = Some(Instr::decode(self.last_opcode));

                self.reset_instr_state(bus);

                self.regs.pc += 1;
            }

            return;
        } else if self.cur_instr.unwrap().mnemonic == Mnemonic::BRK {
            self.cur_int = Some(&InterruptType::BRK);
            self.exec_interrupt(mem);
            return;
        } else if self.instr_cycle == 2 && self.cur_instr.unwrap().addr_mode != AddrMode::IMP
                && self.cur_instr.unwrap().addr_mode != AddrMode::IMM {
            // special case
            if self.cur_instr.unwrap().addr_mode == AddrMode::REL {
                self.poll_interrupts();
            }


            // this doesn't execute for implicit/immediate instructions because
            // they have additional steps beyond fetching on this cycle
            self.cur_operand |= self.next_prg_byte(mem) as u16; // fetch low byte of operand
            self.regs.pc += 1;
            return;
        } else {
            use InstrType::*;
            use AddrMode::*;
            match self.cur_instr.unwrap().mnemonic.get_type() {
                Jump => {
                    if self.cur_instr.unwrap().mnemonic == Mnemonic::JSR {
                        self.handle_jsr(mem);
                    } else {
                        assert!(self.cur_instr.unwrap().mnemonic == Mnemonic::JMP);
                        self.handle_jmp(mem);
                    }
                    return;
                }
                Return => {
                    if self.cur_instr.unwrap().mnemonic == Mnemonic::RTI {
                        self.handle_rti(mem);
                    } else {
                        self.handle_rts(mem);
                    }
                    return;
                }
                Branch => {
                    self.handle_branch(mem, bus);
                    return;
                }
                Stack => {
                    self.handle_stack_instr(mem);
                }
                _ => match self.cur_instr.unwrap().addr_mode {
                    IMP => {
                        self.assert_cycle(2, 2);

                        match self.cur_instr.unwrap().mnemonic.get_type() {
                            Read => {
                                bus.write(self.regs.acc);
                                self.do_instr_operation(bus);
                            }
                            Write => {
                                self.do_instr_operation(bus);
                                self.regs.acc = bus.read();
                            }
                            ReadWrite => {
                                bus.write(self.regs.acc);
                                self.do_instr_operation(bus);
                                self.regs.acc = bus.read();
                            }
                            Stack | Register | Return | Other => {
                                self.do_instr_operation(bus);
                            }
                            _ => panic!("Encountered unexpected mnemonic {:?}", self.cur_instr.unwrap().mnemonic)
                        }

                        self.instr_cycle = 0; // reset for next instruction
                    }
                    IMM => {
                        self.assert_cycle(2, 2);

                        self.cur_operand |= self.next_prg_byte(mem) as u16; // fetch immediate byte
                        self.regs.pc += 1; // increment PC

                        bus.write(self.cur_operand as u8);
                        self.do_instr_operation(bus);

                        self.instr_cycle = 0; // reset for next instruction
                    }
                    ZRP => self.handle_instr_zrp(mem, bus),
                    ZPX | ZPY => self.handle_instr_zpi(mem, bus),
                    ABS => self.handle_instr_abs(mem, bus),
                    ABX | ABY => self.handle_instr_abi(mem, bus),
                    IZX => self.handle_instr_izx(mem, bus),
                    IZY => self.handle_instr_izy(mem, bus),
                    _ => panic!("Unexpected addressing mode {:?}", self.cur_instr.unwrap().addr_mode)
                }
            }
        }
    }

    pub fn cycle(&mut self, mem: &mut impl SysMemIface, bus: &mut impl SysBusIface, int_lines: &impl SysIntLinesIface) -> () {
        self.do_instr_cycle(mem, bus);

        if self.queued_int.is_none() && self.instr_cycle == 0
                && self.cur_instr.is_some() && self.cur_instr.unwrap().addr_mode == AddrMode::REL {
            self.poll_interrupts();
        }

        self.read_interrupt_lines(int_lines);

        self.instr_cycle += 1;
    }

    fn print_cur_instr(&self, bus: &impl SysBusIface) -> () {
        if self.log_callback.is_none() || self.cur_instr.is_none() {
            return;
        }

        let str_machine_code = match self.cur_instr.unwrap().cycle_count() {
            1 => format!("{:02X}      ", self.last_opcode),
            2 => format!("{:02X} {:02X}   ", self.last_opcode, self.cur_operand & 0xFF),
            3 => format!("{:02X} {:02X} {:02X}", self.last_opcode, self.cur_operand & 0xFF, self.cur_operand >> 8),
            _ => panic!("Unexpected cycle count {}", self.cur_instr.unwrap().cycle_count())
        };
    
        let instr_type = self.cur_instr.as_ref().unwrap().mnemonic.get_type();
        let str_param = match self.cur_instr.as_ref().unwrap().addr_mode {
            AddrMode::IMM => format!("#${:02X}                   ", self.cur_operand & 0xFF),
            AddrMode::ZRP => match instr_type {
                InstrType::Read | InstrType::ReadWrite => format!("${:02X}              -> ${:02X}",
                    self.cur_operand & 0xFF, bus.read()),
                _ => format!("${:02X}              <- ${:02X}",
                    self.cur_operand & 0xFF, bus.read())
            }
            AddrMode::ZPX | AddrMode::ZPY => match instr_type {
                InstrType::Read => format!("${:02X},{}   -> ${:04X} -> ${:02X}",
                    self.cur_operand & 0xFF,
                    if self.cur_instr.unwrap().addr_mode == AddrMode::ZPX { "X" } else { "Y" },
                    self.eff_operand,
                    bus.read()),
                _ => format!("${:02X},{}   -> ${:04X} <- ${:02X}",
                    self.cur_operand & 0xFF,
                    if self.cur_instr.unwrap().addr_mode == AddrMode::ZPX { "X" } else { "Y" },
                    self.eff_operand,
                    bus.read())
            }
            AddrMode::ABS => match instr_type {
                InstrType::Read => format!("${:04X}            -> ${:02X}", self.cur_operand, bus.read()),
                _ => format!("${:04X}            <- ${:02X}", self.cur_operand, bus.read())
            }
            AddrMode::ABX | AddrMode::ABY => match instr_type {
                InstrType::Read => format!("${:04X},{} -> ${:04X} -> ${:02X}",
                    self.cur_operand,
                    if self.cur_instr.unwrap().addr_mode == AddrMode::ABX { "X" } else { "Y" },
                    self.eff_operand,
                    bus.read()),
                _ => format!("${:04X},{} -> ${:04X} <- ${:02X}",
                    self.cur_operand,
                    if self.cur_instr.unwrap().addr_mode == AddrMode::ABX { "X" } else { "Y" },
                    self.eff_operand,
                    bus.read())
            }
            AddrMode::REL => format!("#${:02X}    -> ${:04X}       ", self.cur_operand & 0xFF, self.eff_operand),
            AddrMode::IND => format!("(${:04X}) -> ${:04X}       ", self.cur_operand, self.eff_operand),
            AddrMode::IZX => format!("(${:02X},X) -> ${:04X} -> ${:02X}", self.cur_operand & 0xFF, self.eff_operand,
                    bus.read()),
            AddrMode::IZY => format!("(${:02X}),Y -> ${:04X} -> ${:02X}", self.cur_operand & 0xFF, self.eff_operand,
                    bus.read()),
            AddrMode::IMP => format!("                       ")
        };
    
        (self.log_callback.unwrap())(&format!("{}  {:?} {}", str_machine_code, self.cur_instr.unwrap().mnemonic, str_param)[..]);
    }
}

#[cfg(test)]
mod cpu_tests;
