#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mnemonic {
    LDA, LDX, LDY, STA, STX, STY, TAX, TAY,
    TSX, TXA, TYA, TXS, ADC, SBC, DEC, DEX,
    DEY, INC, INX, INY, AND, ASL, LSR, BIT,
    EOR, ORA, ROL, ROR, BCC, BCS, BNE, BEQ,
    BPL, BMI, BVC, BVS, JMP, JSR, RTI, RTS,
    CLC, CLD, CLI, CLV, CMP, CPX, CPY, SEC,
    SED, SEI, PHA, PHP, PLA, PLP, BRK, NOP,
    KIL, ANC, SLO, RLA, SRE, RRA, SAX, LAX,
    DCP, ALR, XAA, TAS, SAY, XAS, AXA, ARR,
    LAS, ISC, AXS
}

#[derive(Clone, Copy, PartialEq)]
pub enum InstrType {
    Other, Branch, Jump, Read, Write,
    ReadWrite, Stack, Register, Return
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AddrMode {
    IMM, ZRP, ZPX, ZPY, ABS, ABX,
    ABY, IND, IZX, IZY, REL, IMP
}

#[derive(Clone, Copy, PartialEq)]
pub struct Instr {
    pub mnemonic: Mnemonic,
    pub addr_mode: AddrMode
}

pub struct InstrParameter {
    pub value: u8,
    pub raw_operand: u16,
    pub adj_operand: u16
}

static INSTR_LIST: [Instr; 256] = {
    const fn mkins(mnemonic: Mnemonic, addr_mode: AddrMode) -> Instr {
        Instr {mnemonic, addr_mode}
    }

    use Mnemonic::*;
    use AddrMode::*;
    [
        mkins(BRK, IMP), mkins(ORA, IZX), mkins(KIL, IMP), mkins(SLO, IZX),
        mkins(NOP, ZRP), mkins(ORA, ZRP), mkins(ASL, ZRP), mkins(SLO, ZRP),
        mkins(PHP, IMP), mkins(ORA, IMM), mkins(ASL, IMP), mkins(ANC, IMM),
        mkins(NOP, ABS), mkins(ORA, ABS), mkins(ASL, ABS), mkins(SLO, ABS),
        mkins(BPL, REL), mkins(ORA, IZY), mkins(KIL, IMP), mkins(SLO, IZY),
        mkins(NOP, ZPX), mkins(ORA, ZPX), mkins(ASL, ZPX), mkins(SLO, ZPX),
        mkins(CLC, IMP), mkins(ORA, ABY), mkins(NOP, IMP), mkins(SLO, ABY),
        mkins(NOP, ABX), mkins(ORA, ABX), mkins(ASL, ABX), mkins(SLO, ABX),
        mkins(JSR, ABS), mkins(AND, IZX), mkins(KIL, IMP), mkins(RLA, IZX),
        mkins(BIT, ZRP), mkins(AND, ZRP), mkins(ROL, ZRP), mkins(RLA, ZRP),
        mkins(PLP, IMP), mkins(AND, IMM), mkins(ROL, IMP), mkins(ANC, IMM),
        mkins(BIT, ABS), mkins(AND, ABS), mkins(ROL, ABS), mkins(RLA, ABS),
        mkins(BMI, REL), mkins(AND, IZY), mkins(KIL, IMP), mkins(RLA, IZY),
        mkins(NOP, ZPX), mkins(AND, ZPX), mkins(ROL, ZPX), mkins(RLA, ZPX),
        mkins(SEC, IMP), mkins(AND, ABY), mkins(NOP, IMP), mkins(RLA, ABY),
        mkins(NOP, ABX), mkins(AND, ABX), mkins(ROL, ABX), mkins(RLA, ABX),
        mkins(RTI, IMP), mkins(EOR, IZX), mkins(KIL, IMP), mkins(SRE, IZX),
        mkins(NOP, ZRP), mkins(EOR, ZRP), mkins(LSR, ZRP), mkins(SRE, ZRP),
        mkins(PHA, IMP), mkins(EOR, IMM), mkins(LSR, IMP), mkins(ALR, IMM),
        mkins(JMP, ABS), mkins(EOR, ABS), mkins(LSR, ABS), mkins(SRE, ABS),
        mkins(BVC, REL), mkins(EOR, IZY), mkins(KIL, IMP), mkins(SRE, IZY),
        mkins(NOP, ZPX), mkins(EOR, ZPX), mkins(LSR, ZPX), mkins(SRE, ZPX),
        mkins(CLI, IMP), mkins(EOR, ABY), mkins(NOP, IMP), mkins(SRE, ABY),
        mkins(NOP, ABX), mkins(EOR, ABX), mkins(LSR, ABX), mkins(SRE, ABX),
        mkins(RTS, IMP), mkins(ADC, IZX), mkins(KIL, IMP), mkins(RRA, IZX),
        mkins(NOP, ZRP), mkins(ADC, ZRP), mkins(ROR, ZRP), mkins(RRA, ZRP),
        mkins(PLA, IMP), mkins(ADC, IMM), mkins(ROR, IMP), mkins(ARR, IMM),
        mkins(JMP, IND), mkins(ADC, ABS), mkins(ROR, ABS), mkins(RRA, ABS),
        mkins(BVS, REL), mkins(ADC, IZY), mkins(KIL, IMP), mkins(RRA, IZY),
        mkins(NOP, ZPX), mkins(ADC, ZPX), mkins(ROR, ZPX), mkins(RRA, ZPX),
        mkins(SEI, IMP), mkins(ADC, ABY), mkins(NOP, IMP), mkins(RRA, ABY),
        mkins(NOP, ABX), mkins(ADC, ABX), mkins(ROR, ABX), mkins(RRA, ABX),
        mkins(NOP, IMM), mkins(STA, IZX), mkins(NOP, IMM), mkins(SAX, IZX),
        mkins(STY, ZRP), mkins(STA, ZRP), mkins(STX, ZRP), mkins(SAX, ZRP),
        mkins(DEY, IMP), mkins(NOP, IMM), mkins(TXA, IMP), mkins(XAA, IMM),
        mkins(STY, ABS), mkins(STA, ABS), mkins(STX, ABS), mkins(SAX, ABS),
        mkins(BCC, REL), mkins(STA, IZY), mkins(KIL, IMP), mkins(AXA, IZY),
        mkins(STY, ZPX), mkins(STA, ZPX), mkins(STX, ZPY), mkins(SAX, ZPY),
        mkins(TYA, IMP), mkins(STA, ABY), mkins(TXS, IMP), mkins(TAS, ABY),
        mkins(SAY, ABX), mkins(STA, ABX), mkins(XAS, ABY), mkins(AXA, ABY),
        mkins(LDY, IMM), mkins(LDA, IZX), mkins(LDX, IMM), mkins(LAX, IZX),
        mkins(LDY, ZRP), mkins(LDA, ZRP), mkins(LDX, ZRP), mkins(LAX, ZRP),
        mkins(TAY, IMP), mkins(LDA, IMM), mkins(TAX, IMP), mkins(LAX, IMM),
        mkins(LDY, ABS), mkins(LDA, ABS), mkins(LDX, ABS), mkins(LAX, ABS),
        mkins(BCS, REL), mkins(LDA, IZY), mkins(KIL, IMP), mkins(LAX, IZY),
        mkins(LDY, ZPX), mkins(LDA, ZPX), mkins(LDX, ZPY), mkins(LAX, ZPY),
        mkins(CLV, IMP), mkins(LDA, ABY), mkins(TSX, IMP), mkins(LAS, ABY),
        mkins(LDY, ABX), mkins(LDA, ABX), mkins(LDX, ABY), mkins(LAX, ABY),
        mkins(CPY, IMM), mkins(CMP, IZX), mkins(NOP, IMM), mkins(DCP, IZX),
        mkins(CPY, ZRP), mkins(CMP, ZRP), mkins(DEC, ZRP), mkins(DCP, ZRP),
        mkins(INY, IMP), mkins(CMP, IMM), mkins(DEX, IMP), mkins(AXS, IMM),
        mkins(CPY, ABS), mkins(CMP, ABS), mkins(DEC, ABS), mkins(DCP, ABS),
        mkins(BNE, REL), mkins(CMP, IZY), mkins(KIL, IMP), mkins(DCP, IZY),
        mkins(NOP, ZPX), mkins(CMP, ZPX), mkins(DEC, ZPX), mkins(DCP, ZPX),
        mkins(CLD, IMP), mkins(CMP, ABY), mkins(NOP, IMP), mkins(DCP, ABY),
        mkins(NOP, ABX), mkins(CMP, ABX), mkins(DEC, ABX), mkins(DCP, ABX),
        mkins(CPX, IMM), mkins(SBC, IZX), mkins(NOP, IMM), mkins(ISC, IZX),
        mkins(CPX, ZRP), mkins(SBC, ZRP), mkins(INC, ZRP), mkins(ISC, ZRP),
        mkins(INX, IMP), mkins(SBC, IMM), mkins(NOP, IMP), mkins(SBC, IMM),
        mkins(CPX, ABS), mkins(SBC, ABS), mkins(INC, ABS), mkins(ISC, ABS),
        mkins(BEQ, REL), mkins(SBC, IZY), mkins(KIL, IMP), mkins(ISC, IZY),
        mkins(NOP, ZPX), mkins(SBC, ZPX), mkins(INC, ZPX), mkins(ISC, ZPX),
        mkins(SED, IMP), mkins(SBC, ABY), mkins(NOP, IMP), mkins(ISC, ABY),
        mkins(NOP, ABX), mkins(SBC, ABX), mkins(INC, ABX), mkins(ISC, ABX)
    ]
};

impl Mnemonic {
    pub fn get_type(&self) -> InstrType {
        use Mnemonic::*;
        match self {
            LDA | LDX | LDY | ADC | SBC | AND | BIT | EOR |
            ORA | CMP | CPX | CPY | NOP | LAX | ANC | ALR |
            XAA | ARR | LAS => InstrType::Read,
            STA | STX | STY | SAX | AXS => InstrType::Write,
            DEC | INC | ASL | LSR | ROL | ROR | SLO | DCP |
            ISC | RLA | SRE | RRA | SAY | XAS | AXA => InstrType::ReadWrite,
            BCC | BCS | BNE | BEQ | BPL | BMI | BVC | BVS => InstrType::Branch,
            JMP | JSR => InstrType::Jump,
            PHA | PLA | PHP | PLP => InstrType::Stack,
            TAX | TAY | TXA | TYA | INX | INY | DEX | DEY |
            TSX | TXS | CLC | SEC | CLI | SEI | CLV | CLD |
            SED | TAS => InstrType::Register,
            RTS | RTI => InstrType::Return,
            BRK | KIL => InstrType::Other
        }
    }
}

impl Instr {
    pub fn decode(opcode: u8) -> &'static Instr {
        &INSTR_LIST[opcode as usize]
    }

    pub fn cycle_count(&self) -> u8 {
        if self.mnemonic == Mnemonic::BRK {
            2
        } else {
            match self.addr_mode {
                AddrMode::IMP => 1,
                AddrMode::ABS | AddrMode::ABX | AddrMode::ABY | AddrMode::IND => 3,
                _ => 2
            }
        }
    }
}
