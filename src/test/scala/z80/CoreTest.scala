package z80

import chisel3._
import chiseltest._
import org.scalatest._

class CoreTest extends FlatSpec with ChiselScalatestTester with Matchers {
  "An add operation" should "add the inputs" in {
    test(new Core) { c =>
      c.io.subtract.poke(0.U)
      c.io.a.poke(3.U)
      c.io.b.poke(2.U)
      c.io.carryIn.poke(0.U)
      c.io.result.expect(5.U)
      c.io.halfCarryOut.expect(0.U)
      c.io.carryOut.expect(0.U)
    }
  }

  it should "add the inputs with the carry bit" in {
    test(new Core) { c =>
      c.io.subtract.poke(0.U)
      c.io.a.poke(3.U)
      c.io.b.poke(2.U)
      c.io.carryIn.poke(1.U)
      c.io.result.expect(6.U)
      c.io.halfCarryOut.expect(0.U)
      c.io.carryOut.expect(0.U)
    }
  }

  it should "set the half-carry bit" in {
    test(new Core) { c =>
      c.io.subtract.poke(0.U)
      c.io.a.poke(15.U)
      c.io.b.poke(1.U)
      c.io.carryIn.poke(0.U)
      c.io.result.expect(16.U)
      c.io.halfCarryOut.expect(1.U)
      c.io.carryOut.expect(0.U)
    }
  }

  it should "set the carry bit" in {
    test(new Core) { c =>
      c.io.subtract.poke(0.U)
      c.io.a.poke(255.U)
      c.io.b.poke(1.U)
      c.io.carryIn.poke(0.U)
      c.io.result.expect(0.U)
      c.io.halfCarryOut.expect(1.U)
      c.io.carryOut.expect(1.U)
    }
  }

  "A subtract operation" should "subtract the inputs" in {
    test(new Core) { c =>
      c.io.subtract.poke(1.U)
      c.io.a.poke(3.U)
      c.io.b.poke(2.U)
      c.io.carryIn.poke(0.U)
      c.io.result.expect(1.U)
      c.io.halfCarryOut.expect(0.U)
      c.io.carryOut.expect(0.U)
    }
  }

  it should "subtract the inputs with the carry bit" in {
    test(new Core) { c =>
      c.io.subtract.poke(1.U)
      c.io.a.poke(3.U)
      c.io.b.poke(2.U)
      c.io.carryIn.poke(1.U)
      c.io.result.expect(0.U)
      c.io.halfCarryOut.expect(0.U)
      c.io.carryOut.expect(0.U)
    }
  }

  it should "set the half-carry bit" in {
    test(new Core) { c =>
      c.io.subtract.poke(1.U)
      c.io.a.poke(16.U)
      c.io.b.poke(1.U)
      c.io.carryIn.poke(0.U)
      c.io.result.expect(15.U)
      c.io.halfCarryOut.expect(1.U)
      c.io.carryOut.expect(0.U)
    }
  }

  it should "set the carry bit" in {
    test(new Core) { c =>
      c.io.subtract.poke(1.U)
      c.io.a.poke(0.U)
      c.io.b.poke(1.U)
      c.io.carryIn.poke(0.U)
      c.io.result.expect(255.U)
      c.io.halfCarryOut.expect(1.U)
      c.io.carryOut.expect(1.U)
    }
  }
}
