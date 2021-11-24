mod m6502;
use m6502::M6502;

fn main() {
    let cpu = M6502 { pc: 0, ac: 0, x: 0, y: 0, sr: 0, sp: 0 };

    println!("{:?}", cpu);
}
