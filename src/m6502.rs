extern crate itertools;
extern crate hex;

use itertools::Itertools;
use hex::decode;
use bitmatch::bitmatch;


#[derive(Clone, Copy, Debug)]
pub struct M6502 {
    pub pc: u16,
    pub ac: u8,
    pub x: u8,
    pub y: u8,
    pub sr: u8, // [NV-BDIZC]
    pub sp: u8,

    pub ram: [u8; 256]
}


/*/ https://llx.com/Neil/a2/opcodes.html
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
}*/

impl M6502 {
    // Instructions
    // aaabbbcc - cc = 01
    pub fn ORA(&self, mode: u8) {}
    pub fn AND(&self, mode: u8) {}
    pub fn EOR(&self, mode: u8) {}
    pub fn ADC(&self, mode: u8) {}
    pub fn STA(&self, mode: u8) {}
    pub fn LDA(&self, mode: u8) {}
    pub fn CMP(&self, mode: u8) {}
    pub fn SBC(&self, mode: u8) {}

    // cc = 10
    pub fn ASL(&self, mode: u8) {}
    pub fn ROL(&self, mode: u8) {}
    pub fn LSR(&self, mode: u8) {}
    pub fn ROR(&self, mode: u8) {}
    pub fn STX(&self, mode: u8) {}
    pub fn LDX(&self, mode: u8) {}
    pub fn DEC(&self, mode: u8) {}
    pub fn INC(&self, mode: u8) {}

    // cc = 00
    pub fn BIT(&self, mode: u8) {}
    pub fn JMP(&self, mode: u8) {}
    pub fn AJMP(&self, mode: u8) {}
    pub fn STY(&self, mode: u8) {}
    pub fn LDY(&self, mode: u8) {}
    pub fn CPY(&self, mode: u8) {}
    pub fn CPX(&self, mode: u8) {}

    // xxy10000; branches x=neg,over,carry zero; cmp(x, y)
    pub fn BPL(&self) {}
    pub fn BMI(&self) {}
    pub fn BVC(&self) {}
    pub fn BVS(&self) {}
    pub fn BCC(&self) {}
    pub fn BCS(&self) {}
    pub fn BNE(&self) {}
    pub fn BEQ(&self) {}

    // no pattern
    pub fn BRK(&self) {}
    pub fn JSR(&self) {}
    pub fn RTI(&self) {}
    pub fn RTS(&self) {}

    pub fn PHP(&self) {}
    pub fn PLP(&self) {}
    pub fn PHA(&self) {}
    pub fn PLA(&self) {}
    pub fn DEY(&self) {}
    pub fn TAY(&self) {}
    pub fn INY(&self) {}
    pub fn INX(&self) {}

    pub fn CLC(&self) {}
    pub fn SEC(&self) {}
    pub fn CLI(&self) {}
    pub fn SEI(&self) {}
    pub fn TYA(&self) {}
    pub fn CLV(&self) {}
    pub fn CLD(&self) {}
    pub fn SED(&self) {}

    pub fn TXA(&self) {}
    pub fn TXS(&self) {}
    pub fn TAX(&self) {}
    pub fn TSX(&self) {}
    pub fn DEX(&self) {}
    pub fn NOP(&self) {}
}

impl M6502 {
    #[bitmatch]
    fn match_instr(&self, instr: u8) {
        #[bitmatch]
        match instr {
            "00000000" => self.BRK(),
            "00100000" => self.JSR(), // absolute JSR
            "01000000" => self.RTI(),
            "01100000" => self.RTS(),
            // cc = 01
            "ooommm01" => match o {
                0x00 => self.ORA(m),   
                0x01 => self.AND(m), 
                0x02 => self.EOR(m), 
                0x03 => self.ADC(m), 
                0x04 => self.STA(m), 
                0x05 => self.LDA(m),  
                0x06 => self.CMP(m),
                0x07 => self.SBC(m),
                _ => self.NOP(),
            },
            // cc = 10
            "ooommm10" => match o {
                0x00 => self.ASL(m),   
                0x01 => self.ROL(m), 
                0x02 => self.LSR(m), 
                0x03 => self.ROR(m), 
                0x04 => self.STX(m), 
                0x05 => self.LDX(m),  
                0x06 => self.DEC(m),
                0x07 => self.INC(m),
                _ => self.NOP()
            },
            // cc = 00
            "ooommm00" => match o {
                0x00 => self.BIT(m),   
                0x01 => self.JMP(m), 
                0x02 => self.AJMP(m), 
                0x03 => self.STY(m), 
                0x04 => self.LDY(m), 
                0x05 => self.CPY(m),  
                0x06 => self.CPX(m),
                _ => self.NOP()
            },
            // conditional jumps = xxy10000
            "00010000" => self.BPL(),
            "00110000" => self.BMI(),
            "01010000" => self.BVC(),
            "01110000" => self.BVS(),
            "10010000" => self.BCC(),
            "10110000" => self.BCS(),
            "11010000" => self.BNE(),
            "11110000" => self.BEQ(),
            // no pattern
            "00001000" => self.PHP(), 
            "00101000" => self.PLP(), 
            "01001000" => self.PHA(), 
            "01101000" => self.PLA(),
            "10001000" => self.DEY(), 
            "10101000" => self.TAY(), 
            "01001100" => self.INY(), 
            "11101000" => self.INX(),
            "00011000" => self.CLC(),
            "00111000" => self.SEC(),
            "01011000" => self.CLI(),
            "01111000" => self.SEI(),
            "10011000" => self.TYA(),
            "10111000" => self.CLV(),
            "11011000" => self.CLD(),
            "11111000" => self.SED(),
            "10001010" => self.TXA(),
            "10011010" => self.TXS(),
            "10101010" => self.TAX(),
            "10111010" => self.TSX(),
            "11001010" => self.DEX(),
            "11101010" => self.NOP(),
            _ => self.NOP(),
        };
    }

    pub fn run(&mut self, prog: &str) {
        let i = 0;

        self.ram[0..prog.len()/2].copy_from_slice(&decode(prog).unwrap());

        // hacked
        for _ in 0..10000 {
            self.match_instr(self.ram[self.pc as usize]);
        }
    }
}

