mod m6502;
use m6502::M6502;

fn main() {
    let mut cpu = M6502 { pc: 0, ac: 0, x: 0, y: 0, sr: 0, sp: 0, ram: [0; 256 * 256]};

    println!("{:?}", cpu);

    cpu.run("A9004828A90008688D8713A9FF4828A90008688D861360");

    println!("{:?}", cpu);
}