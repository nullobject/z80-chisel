import org.scalatest._
import chiseltest._
import chisel3._

class ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  it should "ADD" in {
    test(new ALU) { c =>
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.op.poke(Ops.add)
      c.io.q.expect(3.U)
    }
  }

  it should "SUB" in {
    test(new ALU) { c =>
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.op.poke(Ops.sub)
      c.io.q.expect(1.U)
    }
  }
}
