package z80

import chisel3._
import chisel3.util._

/**
 * Operations
 */
object Ops {
  val nop :: add :: sub :: Nil = Enum(3)
}

class Flags extends Bundle {
  val sign = UInt(1.W)
  val zero = UInt(1.W)
  val unused2 = UInt(1.W)
  val half = UInt(1.W)
  val unused1 = UInt(1.W)
  val parity = UInt(1.W)
  val negative = UInt(1.W)
  val carry = UInt(1.W)
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

  val flags = Wire(new Flags)
  flags.carry := result(8)
  flags.negative := 0.U
  flags.parity := 0.U
  flags.unused1 := 0.U
  flags.half := 0.U
  flags.unused2 := 0.U
  flags.zero := result(7, 0) === 0.U
  flags.sign := 0.U

  io.q := result(7, 0)
  io.flags := flags.asUInt()
}
