mod m6502;
use m6502::M6502;

fn main() {
    let mut cpu = M6502 { pc: Box::new(0), 
                          ac: Box::new(0), 
                          x: Box::new(0), 
                          y: Box::new(0), 
                          sr: Box::new(0), 
                          sp: Box::new(0), 
                          ram: [0; 256 * 256]};

    println!("{:?}", cpu);

    cpu.run("A9004828A90008688D8713A9FF4828A90008688D861360");

    println!("{:?}", cpu);
}