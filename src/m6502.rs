use bitmatch::bitmatch;


#[derive(Clone, Copy, Debug)]
pub struct M6502 {
    pub pc: u16,
    pub ac: u8,
    pub x: u8,
    pub y: u8,
    pub sr: u8, // [NV-BDIZC]
    pub sp: u8 
}


// https://llx.com/Neil/a2/opcodes.html
#[derive(Debug)]
pub enum Instr {
    BRK, JSR, RTI, RTS,
    // cc = 01
    ORA(u8), AND(u8), EOR(u8), ADC(u8),
    STA(u8), LDA(u8), CMP(u8), SBC(u8),
    // cc = 10
    ASL(u8), ROL(u8), LSR(u8), ROR(u8),
    STX(u8), LDX(u8), DEC(u8), INC(u8),  
    // cc = 00
    BIT(u8), JMP(u8), AJMP(u8), STY(u8),
    LDY(u8), CPY(u8), CPX(u8),          
    // xxy10000; branches x=neg,over,carry zero; cmp(x, y)
    BPL, BMI, BVC, BVS, BCC, BCS, BNE, BEQ,
    // nopatt
    PHP, PLP, PHA, PLA, DEY, TAY, INY, INX,
    CLC, SEC, CLI, SEI, TYA, CLV, CLD, SED,
    TXA, TXS, TAX, TSX, DEX, NOP
}


impl M6502 {
    #[bitmatch]
    pub fn match_instr(&self, instr: u8) -> Instr {
        use crate::m6502::Instr::*;

        #[bitmatch]
        match instr {
            "00000000" => BRK,
            "00100000" => JSR, // absolute JSR
            "01000000" => RTI,
            "01100000" => RTS,
            // cc = 01
            "ooommm01" => match o {
                0x00 => ORA(m),   
                0x01 => AND(m), 
                0x02 => EOR(m), 
                0x03 => ADC(m), 
                0x04 => STA(m), 
                0x05 => LDA(m),  
                0x06 => CMP(m),
                0x07 => SBC(m),
                _ => NOP,
            },
            // cc = 10
            "ooommm10" => match o {
                0x00 => ASL(m),   
                0x01 => ROL(m), 
                0x02 => LSR(m), 
                0x03 => ROR(m), 
                0x04 => STX(m), 
                0x05 => LDX(m),  
                0x06 => DEC(m),
                0x07 => INC(m),
                _ => NOP
            },
            // cc = 00
            "ooommm00" => match o {
                0x00 => BIT(m),   
                0x01 => JMP(m), 
                0x02 => AJMP(m), 
                0x03 => STY(m), 
                0x04 => LDY(m), 
                0x05 => CPY(m),  
                0x06 => CPX(m),
                _ => NOP
            },
            // conditional jumps = xxy10000
            "xxy10000" => match x { 
                // [!] Assumes all cond jumps are the same.
                // Negative
                0x0 => if self.sr & 0b10000000 == y && y == 0 { BPL } else { BMI },
                // Overflow
                0x1 => if self.sr & 0b01000000 == y && y == 0 { BVC } else { BVS },
                // Carry
                0x2 => if self.sr & 0b00000001 == y && y == 0 { BCC } else { BCS },
                // Zero
                0x3 => if self.sr & 0b00000010 == y && y == 0 { BNE } else { BEQ },
                _ => NOP
            },
            "00001000" => PHP, 
            "00101000" => PLP, 
            "01001000" => PHA, 
            "01101000" => PLA,
            "10001000" => DEY, 
            "10101000" => TAY, 
            "01001100" => INY, 
            "11101000" => INX,
            "00011000" => CLC,
            "00111000" => SEC,
            "01011000" => CLI,
            "01111000" => SEI,
            "10011000" => TYA,
            "10111000" => CLV,
            "11011000" => CLD,
            "11111000" => SED,
            "10001010" => TXA,
            "10011010" => TXS,
            "10101010" => TAX,
            "10111010" => TSX,
            "11001010" => DEX,
            "11101010" => NOP,
            _ => NOP,
        }
    }
}

