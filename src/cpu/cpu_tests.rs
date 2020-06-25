use crate::*;

use std::fs::File;
use std::io::Read;

struct TestSystem {
    sys_ram: [u8; 0x800],
    sys_bus: u8,
    program_rom: Vec<u8>,
    cpu: cpu::Cpu,
}

impl cpu::SysMemIface for TestSystem {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.sys_ram[(addr as usize % self.sys_ram.len())],
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
            self.sys_ram[addr as usize % self.sys_ram.len()] = val;
        }
    }
}

impl cpu::SysBusIface for TestSystem {
    fn read(&self) -> u8 {
        return self.sys_bus;
    }

    fn write(&mut self, val: u8) -> () {
        self.sys_bus = val;
    }
}

impl cpu::SysIntLinesIface for TestSystem {
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
    fn new(file_name: &str) -> TestSystem {
        let mut sys: TestSystem = TestSystem {
            sys_ram: [0; 0x800],
            sys_bus: 0,
            program_rom: Vec::with_capacity(0),
            cpu: Default::default(),
        };

        let cpu = &mut sys.cpu;
        *cpu = cpu::Cpu::new();

        let file_res = File::open(file_name);
        if file_res.is_err() {
            panic!(
                "Failed to open test file {}: {}",
                file_name,
                file_res.err().unwrap()
            );
        }
        let mut file = file_res.unwrap();

        let read_res = file.read_to_end(&mut sys.program_rom);

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
            //cpu.cycle(self, self, self);

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

    fn run_tests(&mut self, assert_sets: &[&[(u8, fn(cpu: &cpu::Cpu) -> u8)]]) {
        for set in assert_sets.iter() {
            self.pump_cpu();
            for (expected, getter) in set.iter() {
                let actual = getter(&self.cpu);
                assert!(*expected == actual, "Expected {}, got {}", expected, actual);
            }
        }
    }
}

use cpu::StatusBitfield;

#[test]
fn test_addition() -> () {
    let mut sys = TestSystem::new("./addition.asm");
    sys.run_tests(&[
        &[
            (0x02, |cpu: &cpu::Cpu| cpu.regs.acc),
            (0, |cpu: &cpu::Cpu| cpu.regs.status.get_bit(cpu::StatusBit::Carry) as u8),
            (0, |cpu: &cpu::Cpu| cpu.regs.status.get_bit(cpu::StatusBit::Zero) as u8),
            (0, |cpu: &cpu::Cpu| cpu.regs.status.get_bit(cpu::StatusBit::Overflow) as u8),
            (0, |cpu: &cpu::Cpu| cpu.regs.status.get_bit(cpu::StatusBit::Negative) as u8),
        ]
    ]);
}
