use crate::*;

use std::fs::File;
use std::io::Read;

struct SysRam {
    ram: [u8; 0x800],
    program_rom: Vec<u8>,
}

struct SysBus {
    val: u8
}

struct SysIntLines {
}

struct TestSystem {
    sys_ram: SysRam,
    sys_bus: SysBus,
    sys_int: SysIntLines,
    cpu: cpu::Cpu,
}

impl cpu::SysMemIface for SysRam {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.ram[(addr as usize % self.ram.len())],
            0x8000..=0xFFFF => {
                let mut adj_addr = addr % 0x8000;

                if adj_addr >= 0x4000 && self.program_rom.len() <= 0x4000 {
                    adj_addr -= 0x4000;
                }

                if adj_addr as usize >= self.program_rom.len() {
                    return 0;
                }

                return self.program_rom[adj_addr as usize];
            }
            _ => 0,
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> () {
        if addr < 0x2000 {
            self.ram[addr as usize % self.ram.len()] = val;
        }
    }
}

impl cpu::SysBusIface for SysBus {
    fn read(&self) -> u8 {
        return self.val;
    }

    fn write(&mut self, val: u8) -> () {
        self.val = val;
    }
}

impl cpu::SysIntLinesIface for SysIntLines {
    fn poll_nmi(&self) -> bool {
        return true;
    }

    fn poll_irq(&self) -> bool {
        return true;
    }

    fn poll_rst(&self) -> bool {
        return true;
    }
}

impl TestSystem {
    fn log_callback(msg: &str) {
        println!("{}", msg);
    }

    fn new(file_name: &str) -> TestSystem {
        let mut sys: TestSystem = TestSystem {
            sys_ram: SysRam {
                ram: [0; 0x800],
                program_rom: Vec::with_capacity(0),
            },
            sys_bus: SysBus {
                val: 0,
            },
            sys_int: SysIntLines { },
            cpu: Default::default(),
        };

        let cpu = &mut sys.cpu;
        *cpu = cpu::Cpu::new();
        cpu.set_log_callback(Some(TestSystem::log_callback));

        let file_res = File::open(file_name);
        if file_res.is_err() {
            panic!(
                "Failed to open test file {}: {}",
                file_name,
                file_res.err().unwrap()
            );
        }
        let mut file = file_res.unwrap();

        let read_res = file.read_to_end(&mut sys.sys_ram.program_rom);

        if read_res.is_err() {
            panic!(
                "Failed to read test file {}: {}",
                file_name,
                read_res.err().unwrap()
            )
        }
        return sys;
    }

    fn pump_cpu(&mut self) -> () {
        loop {
            let cpu = &mut self.cpu;
            cpu.cycle(&mut self.sys_ram, &mut self.sys_bus, &self.sys_int);

            if cpu.instr_cycle != 1 {
                continue;
            }

            match self.cpu.cur_instr {
                Some(instrs::Instr {
                    mnemonic: instrs::Mnemonic::NOP,
                    addr_mode: _,
                }) => {
                    break;
                }
                _ => (),
            }
        }
    }

    fn run_tests(&mut self, assert_sets: &[&[(u8, fn(sys: &mut TestSystem) -> u8)]]) {
        let mut i = 0;
        for set in assert_sets.iter() {
            self.pump_cpu();
            let mut j = 0;
            for (expected, getter) in set.iter() {
                let actual = getter(self);
                assert!(*expected == actual, "Expected {}, got {} for test ({}, {})", expected, actual, i, j);
                j += 1;
            }
            i += 1;
        }
    }
}

use cpu::StatusBitfield;
use cpu::SysMemIface;

#[test]
fn test_addition() -> () {
    let mut sys = TestSystem::new("./res/tests/addition.bin");
    sys.run_tests(&[
        &[
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ],
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ],
        &[
            (0x80, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ],
        &[
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ],
        &[
            (0x80, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ]
    ]);
}

#[test]
fn test_arithmetic() -> () {
    let mut sys = TestSystem::new("./res/tests/arithmetic.bin");
    sys.run_tests(&[
        // test INX
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x80, |sys: &mut TestSystem| sys.cpu.regs.x),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x81, |sys: &mut TestSystem| sys.cpu.regs.x),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test INY
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.y),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.y),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x80, |sys: &mut TestSystem| sys.cpu.regs.y),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x81, |sys: &mut TestSystem| sys.cpu.regs.y),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test INC
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x80, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x81, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test DEX
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xFF, |sys: &mut TestSystem| sys.cpu.regs.x),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x7F, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test DEY
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.y),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.y),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xFF, |sys: &mut TestSystem| sys.cpu.regs.y),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x7F, |sys: &mut TestSystem| sys.cpu.regs.y),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test DEC
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xFF, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x7F, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
    ]);
}

#[test]
fn test_branch() -> () {
    let mut sys = TestSystem::new("./res/tests/branch.bin");
    sys.run_tests(&[
        // test JMP (indirect)
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.x),
        ],
        // test JMP (indirect)
        &[
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.x),
        ],
        // test JMP
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.x),
        ],
        // test JSR
        &[
            (0x07, |sys: &mut TestSystem| sys.cpu.regs.x),
        ],
        // test BEQ, BNE
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.y),
        ],
        // test BCS, BCC
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.y),
        ],
        // test BPL, BMI
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.y),
        ],
        // test BVS, BVC
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.y),
        ],
    ]);
}

#[test]
fn test_interrupt() -> () {
    let mut sys = TestSystem::new("./res/tests/interrupt.bin");
    sys.run_tests(&[
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.y),
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
        ],
    ]);
}

#[test]
fn test_logic() -> () {
    let mut sys = TestSystem::new("./res/tests/logic.bin");
    sys.run_tests(&[
        // test AND
        &[
            (12, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x80, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test EOR
        &[
            (12, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xCC, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test ORA
        &[
            (0x3F, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xFF, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test ASL
        &[
            (14, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xFE, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (2, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test ASL (memory)
        &[
            (14, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xFE, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test LSR
        &[
            (3, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (3, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test ROL
        &[
            (14, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (15, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xFF, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (3, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test LSR
        &[
            (3, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x83, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x83, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
    ]);
}

#[test]
fn test_stack() -> () {
    let mut sys = TestSystem::new("./res/tests/stack.bin");
    sys.run_tests(&[
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0xFD, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.y),
            (0xFF, |sys: &mut TestSystem| sys.cpu.regs.sp),
        ],
        &[
            (0x01, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.x),
            (0x02, |sys: &mut TestSystem| sys.cpu.regs.y),
            (0xFF, |sys: &mut TestSystem| sys.cpu.regs.sp),
        ],
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::InterruptDisable) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ],
    ]);
}

#[test]
fn test_status() -> () {
    let mut sys = TestSystem::new("./res/tests/status.bin");
    sys.run_tests(&[
                // test explicit flag-setting
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::InterruptDisable) as u8),
        ],
        // test explicit flag-clearing
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::InterruptDisable) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
        ],
        // test comparison instructions
        // test CMP
        // a = v
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // a > v
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // a < v
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // a > v && a is neg && v is neg
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // a > v && a is neg && v is pos
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // a < v && a is neg && v is neg
        &[
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test CPX
        // x == v
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        // test CPY
        // y == v
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
    ]);
}

#[test]
fn test_store_load() -> () {
    let mut sys = TestSystem::new("./res/tests/store_load.bin");
    sys.run_tests(&[
        // ACCUMULATOR TESTS
        // test zero-page addressing
        &[
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x10)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x90)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0xFF)),
        ],
        // test zero-page loading
        &[
            (1, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.x),
            (1, |sys: &mut TestSystem| sys.cpu.regs.y),
        ],
        // test zero-page (x-indexed) addressing
        &[
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x12)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x92)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x01)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0xA1)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x02)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x11)),
        ],
        // test absolute addressing
        &[
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0023)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0303)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0103)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0203)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0303)),
        ],
        // test absolute (x-indexed) addressing
        &[
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0025)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0305)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0105)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0205)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0305)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0005)),
        ],
        // test absolute (y-indexed) addressing
        &[
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0026)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0006)),
        ],
        // test indexed indirect addressing
        &[
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0213)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x1302)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0302)),
        ],
        // test indirect indexed addressing
        &[
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x0215)),
            (1, |sys: &mut TestSystem| sys.sys_ram.read(0x1304)),
        ],
        // X REGISTER TESTS
        // test zero-page addressing
        &[
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0x10)),
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0x90)),
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0xFF)),
        ],
        // test zero-page (y-indexed) addressing
        &[
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0x14)),
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0x94)),
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0x03)),
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0xA0)),
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0x00)),
            (2, |sys: &mut TestSystem| sys.sys_ram.read(0x20)),
        ],
        // Y REGISTER TESTS
        // test zero-page addressing
        &[
            (4, |sys: &mut TestSystem| sys.sys_ram.read(0x10)),
            (4, |sys: &mut TestSystem| sys.sys_ram.read(0x90)),
            (4, |sys: &mut TestSystem| sys.sys_ram.read(0xFF)),
        ],
        // transfer tests
        &[
            (0x42, |sys: &mut TestSystem| sys.sys_ram.read(0x08)),
            (0x52, |sys: &mut TestSystem| sys.sys_ram.read(0x09)),
        ],
    ]);
}

#[test]
fn test_subtraction() -> () {
    let mut sys = TestSystem::new("./res/tests/subtraction.bin");
    sys.run_tests(&[
        &[
            (0x10, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x0F, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x00, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xF0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0xA0, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
        &[
            (0x60, |sys: &mut TestSystem| sys.cpu.regs.acc),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
            (1, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |sys: &mut TestSystem| sys.cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
        ],
    ]);
}
