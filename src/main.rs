mod m6502;
use m6502::M6502;

use std::mem;
use std::ptr;

macro_rules! make_array {
    ($n:expr, $constructor:expr) => {{
        let mut items: [_; $n] = mem::uninitialized();
        for (i, place) in items.iter_mut().enumerate() {
            ptr::write(place, $constructor(i));
        }
        items
    }}
}

fn main() {
    const SIZE: usize = 65536;

    let mut cpu = M6502 { pc: Box::new(0), 
                          ac: Box::new(0), 
                          x: Box::new(0), 
                          y: Box::new(0), 
                          sr: Box::new(0), 
                          sp: Box::new(0), 
                          ram:  unsafe { make_array!(SIZE, |_| Box::new(0))}};

    println!("{:?}", cpu);

    cpu.run("A9004828A90008688D8713A9FF4828A90008688D861360");

    println!("{:?}", cpu);
}