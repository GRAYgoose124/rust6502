extern crate itertools;
extern crate hex;

use hex::decode;
use bitmatch::bitmatch;
use std::fmt;


#[derive(Clone)]
pub struct M6502 {
    pub pc: Box<u16>,
    pub ac: Box<u8>,
    pub x: Box<u8>,
    pub y: Box<u8>,
    pub sr: Box<u8>, // [NV-BDIZC]
    pub sp: Box<u8>,

    pub ram: [Box<u8>; 256 * 256]
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

enum Mode {
    ACC, IMM, ZPG, ZPX, ZPY, REL, ABS, ABX, ABY, IND, IXD, IDX,
    UKN
}

impl M6502 {
    fn push(&mut self, byte: u8) {
        *self.sp = *self.sp + 1;
        *self.ram[0x100+*self.sp as usize] = byte;
    }

    fn pull(&mut self) -> u8 {
        let sp = *self.sp;
        *self.sp = *self.sp - 1;

        *self.ram[0x100+sp as usize]
    }

    fn status(&self, n: char) -> u8 {
        match n {
            'N' => { 0b10000000 },
            'V' => { 0b01000000 },
            '-' => { 0b00100000 },
            'B' => { 0b00010000 },
            'D' => { 0b00001000 },
            'I' => { 0b00000100 },
            'Z' => { 0b00000010 },
            'C' => { 0b00000001 },
            _ => { 0 }
        }
    }

    fn mode(&mut self, mode: Mode) -> &mut Box<u8> {
        match mode {
            Mode::ACC => { &mut self.ac },
            Mode::IMM => { &mut self.ram[(*self.pc+1) as usize]},
            Mode::ZPG => { &mut self.ram[0x0000 + *self.ram[(*self.pc+1) as usize] as usize] },
            Mode::ZPX => { &mut self.ram[(0x0000 + *self.ram[(*self.pc+1) as usize] + *self.x) as usize] },
            Mode::ZPY => { &mut self.ram[(0x0000 + *self.ram[(*self.pc+1) as usize] + *self.y) as usize] },
            Mode::REL => { &mut self.ram[(*self.pc as i16 + ((*self.pc+1) as i8) as i16) as usize]},
            Mode::ABS => { &mut self.ram[((*self.pc+1) + (*self.pc+2)<<1) as usize]},
            Mode::ABX => { &mut self.ram[((*self.pc+1) + (*self.pc+2)<<1 + *self.x) as usize]},
            Mode::ABY => { &mut self.ram[((*self.pc+1) + (*self.pc+2)<<1 + *self.y) as usize]},
            Mode::IND => { 
                let lsb = *self.ram[((*self.pc+1) + (*self.pc+2)<<1) as usize];
                let hsb = *self.ram[((*self.pc+1) + (*self.pc+2)<<1 + 1) as usize];
                &mut self.ram[(lsb as u16 + (hsb as u16) << 1)  as usize]
            }
            Mode::IXD => {
                let lsb = *self.ram[(*self.pc+1) as usize] + *self.x;
                let hsb = *self.ram[(*self.pc+1) as usize] + *self.x;
                &mut self.ram[((lsb as u16 + (hsb as u16) << 1) % 0xFF)  as usize]
            },
            Mode::IDX => {
                let lsb = *self.ram[(*self.pc+1) as usize];
                let hsb = *self.ram[(*self.pc+1) as usize] + 1;
                &mut self.ram[((*self.ram[(lsb as u16 + (hsb as u16) << 1)  as usize] + *self.y) % 0xFF) as usize]
            },
            _ => { &mut self.ac }
        }
    }

    fn mode01(&mut self, mode: u8 ) -> &mut Box<u8> {
        match mode {
            0 => { self.mode(Mode::IXD) }, // Mode::ZPXP
            1 => { self.mode(Mode::ZPG) }, // Mode::ZP
            2 => { self.mode(Mode::IMM) }, // Mode::IMM
            3 => { self.mode(Mode::ABS) }, // Mode::ABS
            4 => { self.mode(Mode::ZPY) }, // Mode::ZPY
            5 => { self.mode(Mode::ZPX) }, // Mode::ZPX
            6 => { self.mode(Mode::ABY) }, // Mode::ABY
            7 => { self.mode(Mode::ABX) }, // Mode::ABX
            _ => { self.mode(Mode::UKN) } // Mode::UKN
        }
    }
}


#[allow(unused_variables, unused_mut)]
impl M6502 {
    // Instructions
    // aaabbbcc - cc = 01
    pub fn ORA(&mut self, mode: u8) { 
        let data = **self.mode01(mode);
        *self.ac = *self.ac | data;

        if *self.ac & 0b10000000 == 1 { 
            *self.sr = *self.sr | self.status('N');
        }
        
        if *self.ac == 0 {
            *self.sr = *self.sr | self.status('Z');
        }
    }
    pub fn AND(&mut self, mode: u8) {
        let data = **self.mode01(mode);
        *self.ac &= data;

        if *self.ac & 0b10000000 == 1 { 
            *self.sr = *self.sr | self.status('N');
        }
        
        if *self.ac == 0 {
            *self.sr = *self.sr | self.status('Z');
        }
    }
    pub fn EOR(&mut self, mode: u8) {
        let data = **self.mode01(mode);
        *self.ac = *self.ac ^ data;

        if *self.ac & 0b10000000 == 1 { 
            *self.sr = *self.sr | self.status('N');
        }
        
        if *self.ac == 0 {
            *self.sr = *self.sr | self.status('Z');
        }
    }
    pub fn ADC(&mut self, mode: u8) {
        let data = **self.mode01(mode);
        let sum = *self.ac as u16 + data as u16 + (*self.sr & self.status('C') ) as u16;
        *self.ac = (sum & 0xFF) as u8;

        if sum > 0xFF {
            *self.sr = *self.sr | self.status('C');
        } else {
            *self.sr = *self.sr & !self.status('C');
        }
    }
    pub fn STA(&mut self, mode: u8) {
        **self.mode01(mode) = *self.ac;
    }
    pub fn LDA(&mut self, mode: u8) {
        *self.ac = **self.mode01(mode);
    }
    pub fn CMP(&mut self, mode: u8) {
        let data = **self.mode01(mode);

    }
    pub fn SBC(&mut self, mode: u8) {}

    // cc = 10
    pub fn ASL(&mut self, mode: u8) {}
    pub fn ROL(&mut self, mode: u8) {}
    pub fn LSR(&mut self, mode: u8) {}
    pub fn ROR(&mut self, mode: u8) {}
    pub fn STX(&mut self, mode: u8) {}
    pub fn LDX(&mut self, mode: u8) {}
    pub fn DEC(&mut self, mode: u8) {}
    pub fn INC(&mut self, mode: u8) {}

    // cc = 00
    pub fn BIT(&mut self, mode: u8) {}
    pub fn JMP(&mut self, mode: u8) {}
    pub fn AJMP(&mut self, mode: u8) {}
    pub fn STY(&mut self, mode: u8) {}
    pub fn LDY(&mut self, mode: u8) {}
    pub fn CPY(&mut self, mode: u8) {}
    pub fn CPX(&mut self, mode: u8) {}

    // xxy10000; conditional branches x=neg,over,carry zero; cmp(x, y)
    fn CBRANCH(&mut self, s: char, n: u8) {
        // S = [NV-BDIZC], n=[01]
        if (*self.sr & self.status(s)) == n { 
            *self.pc = *self.ram[*self.pc as usize] as u16;
        }
    }
    pub fn BPL(&mut self) { self.CBRANCH('N', 0) }
    pub fn BMI(&mut self) { self.CBRANCH('N', 1) }
    pub fn BVC(&mut self) { self.CBRANCH('V', 0) }
    pub fn BVS(&mut self) { self.CBRANCH('V', 1) }
    pub fn BCC(&mut self) { self.CBRANCH('C', 0) }
    pub fn BCS(&mut self) { self.CBRANCH('C', 1) }
    pub fn BNE(&mut self) { self.CBRANCH('Z', 0) }
    pub fn BEQ(&mut self) { self.CBRANCH('Z', 1) }

    // no pattern
    pub fn BRK(&mut self) {}
    pub fn JSR(&mut self) {}
    pub fn RTI(&mut self) {}
    pub fn RTS(&mut self) {}

    pub fn PHP(&mut self) {}
    pub fn PLP(&mut self) {}
    pub fn PHA(&mut self) {}
    pub fn PLA(&mut self) {}
    pub fn DEY(&mut self) {}
    pub fn TAY(&mut self) {}
    pub fn INY(&mut self) {}
    pub fn INX(&mut self) {}

    pub fn CLC(&mut self) {}
    pub fn SEC(&mut self) {}
    pub fn CLI(&mut self) {}
    pub fn SEI(&mut self) {}
    pub fn TYA(&mut self) {}
    pub fn CLV(&mut self) {}
    pub fn CLD(&mut self) {}
    pub fn SED(&mut self) {}

    pub fn TXA(&mut self) {}
    pub fn TXS(&mut self) {}
    pub fn TAX(&mut self) {}
    pub fn TSX(&mut self) {}
    pub fn DEX(&mut self) {}
    pub fn NOP(&mut self) {}
}


impl M6502 {
    #[bitmatch]
    fn match_instr(&mut self, loc: usize) {
        #[bitmatch]
        match *self.ram[loc] {
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
        for (i, byte) in decode(prog).unwrap().iter().enumerate() {
            *(self.ram[(0x0200+i) as usize]) = *byte;
        }

        // hacked
        *self.pc = 0x0200;
        for _ in 0..(prog.len() / 2) {
            self.match_instr(*self.pc as usize);
            *self.pc += 1;
            //if *self.pc <= 0xFFFF { *self.pc += 1 };
        }
    }
}

impl fmt::Debug for M6502 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("M6502")
            .field("pc", &format_args!("0x{:04x}", *self.pc))
            .field("ac", &format_args!("0x{:02x}", *self.ac))
            .field("x",  &format_args!("0x{:02x}", *self.x ))
            .field("y",  &format_args!("0x{:02x}", *self.y ))
            .field("sr", &format_args!("0x{:02x}", *self.sr))
            .field("sp", &format_args!("0x{:02x}", *self.sp))
            .finish()
    }
}
