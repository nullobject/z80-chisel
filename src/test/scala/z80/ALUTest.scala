package z80

import chisel3._
import chiseltest._
import org.scalatest._

class ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ADD"

  it should "add the inputs" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect(3.U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(0.U)
      c.io.b.poke(0.U)
      c.io.flagsIn.poke("b0000_0000".U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0000".U)
    }
  }

  it should "set the half-carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(15.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0000".U)
      c.io.result.expect(16.U)
      c.io.flagsOut.expect("b0001_0000".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(255.U)
      c.io.b.poke(2.U)
      c.io.flagsIn.poke("b0000_0000".U)
      c.io.result.expect(1.U)
      c.io.flagsOut.expect("b0001_0001".U)
    }
  }
  behavior of "ADC"

  it should "add the inputs and carry bit" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.adc)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect(4.U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  behavior of "SUB"

  it should "subtract the inputs" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect(1.U)
      c.io.flagsOut.expect("b0000_0010".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(1.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0000".U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0010".U)
    }
  }

  it should "set the half-carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(16.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0000".U)
      c.io.result.expect(15.U)
      c.io.flagsOut.expect("b0001_0010".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(0.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0000".U)
      c.io.result.expect(255.U)
      c.io.flagsOut.expect("b0001_0011".U)
    }
  }
}
