import org.scalatest._
import chiseltest._
import chisel3._

class ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  it should "do something" in {
    test(new ALU) { c =>
      c.io.a.poke(1.U)
      c.io.b.poke(2.U)
      c.io.c.expect(3.U)
    }
  }
}
