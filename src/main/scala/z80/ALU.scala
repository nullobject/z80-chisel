package z80

import chisel3._
import chisel3.util._

/**
 * Operations
 */
object Ops {
  val nop :: add :: sub :: Nil = Enum(3)
}

object Flags {
  val C = 0
  val N = 1
  val P = 2
  val X = 3
  val H = 4
  val Y = 5
  val Z = 6
  val S = 7
}

class ALU extends Module {
  val io = IO(new Bundle {
    /**
     * The operation to be executed
     */
    val op = Input(UInt(4.W))

    /**
     * Operand A
     */
    val a = Input(UInt(8.W))

    /**
     * Operand B
     */
    val b = Input(UInt(8.W))

    /**
     * The result of the operation
     */
    val q = Output(UInt(8.W))

    /**
     * Flags containing metadata about the result of the operation
     */
    val flags = Output(Bits(8.W))
  })

  // default value
  val result = WireDefault(0.U(9.W))

  switch (io.op) {
    is (Ops.nop) {
      result := io.b
    }
    is (Ops.add) {
      result := io.a +& io.b
    }
    is (Ops.sub) {
      result := io.a -& io.b
    }
  }

  val carry = result(8)
  val zero = result(7, 0) === 0.U

  io.q := result(7, 0)
  io.flags := Cat(zero, 0.U(6.W), carry)
}
