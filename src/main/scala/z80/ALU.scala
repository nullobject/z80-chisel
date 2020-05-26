package z80

import chisel3._
import chisel3.util._

/**
 * Operations
 */
object Ops {
  val add :: adc :: sub :: sbc :: Nil = Enum(4)
}

/**
 * Processor flags
 */
class Flags extends Bundle {
  val sign = UInt(1.W)
  val zero = UInt(1.W)
  val unused2 = UInt(1.W)
  val half = UInt(1.W)
  val unused1 = UInt(1.W)
  val parity = UInt(1.W)
  val subtract = UInt(1.W)
  val carry = UInt(1.W)
}

/**
 * Arithmetic logic unit
 */
class ALU extends Module {
  val io = IO(new Bundle {
    val op = Input(UInt(4.W))
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val flagsIn = Input(Bits(8.W))
    val result = Output(UInt(8.W))
    val flagsOut = Output(Bits(8.W))
  })

  val result = WireDefault(0.U(9.W))
  val flagsIn = io.flagsIn.asTypeOf(new Flags)
  val flagsOut = io.flagsIn.asTypeOf(new Flags)

  // set flags
  flagsOut.zero := result(7, 0) === 0.U
  flagsOut.carry := result(8)

  switch (io.op) {
    is (Ops.add) {
      result := io.a +& io.b
      flagsOut.half := (io.a(3, 0) +& io.b(3, 0))(4)
    }
    is (Ops.adc) {
      result := io.a +& io.b + flagsIn.carry
      flagsOut.half := (io.a(3, 0) +& io.b(3, 0))(4)
    }
    is (Ops.sub) {
      result := io.a -& io.b
      flagsOut.half := (io.a(3, 0) -& io.b(3, 0))(4)
      flagsOut.subtract := 1.U
    }
    is (Ops.sbc) {
      result := io.a -& io.b - flagsIn.carry
      flagsOut.half := (io.a(3, 0) -& io.b(3, 0))(4)
      flagsOut.subtract := 1.U
    }
  }

  io.result := result(7, 0)
  io.flagsOut := flagsOut.asUInt()
}
