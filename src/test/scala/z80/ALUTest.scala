package z80

import chisel3._
import chiseltest._
import org.scalatest._

class ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "NOP"

  it should "output B" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.nop)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.q.expect(1.U)
      c.io.flags.expect("b0000_0000".U)
    }
  }

  behavior of "ADD"

  it should "output A + B" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.q.expect(3.U)
      c.io.flags.expect("b0000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(0.U)
      c.io.b.poke(0.U)
      c.io.q.expect(0.U)
      c.io.flags.expect("b1000_0000".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(255.U)
      c.io.b.poke(2.U)
      c.io.q.expect(1.U)
      c.io.flags.expect("b0000_0001".U)
    }
  }

  behavior of "SUB"

  it should "output A - B" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.q.expect(1.U)
      c.io.flags.expect("b0000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(1.U)
      c.io.b.poke(1.U)
      c.io.q.expect(0.U)
      c.io.flags.expect("b1000_0000".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(0.U)
      c.io.b.poke(1.U)
      c.io.q.expect(255.U)
      c.io.flags.expect("b0000_0001".U)
    }
  }
}
