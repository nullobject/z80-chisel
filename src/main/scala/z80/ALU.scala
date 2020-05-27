package z80

import chisel3._
import chisel3.util._

/**
 * Operations
 */
object Ops {
  val add :: adc :: sub :: sbc :: and :: xor :: or :: cp :: Nil = Enum(8)
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

  val flagsIn = io.flagsIn.asTypeOf(new Flags)
  val flagsOut = io.flagsIn.asTypeOf(new Flags)
  val result = WireDefault(0.U(8.W))

  // arithmetic core for addition/subtraction
  val core = Module(new Core)
  core.io.subtract := 0.U
  core.io.a := io.a
  core.io.b := io.b
  core.io.carryIn := 0.U

  switch (io.op) {
    is (Ops.add) {
      result := core.io.result
    }
    is (Ops.adc) {
      core.io.carryIn := flagsIn.carry
      result := core.io.result
    }
    is (Ops.sub) {
      core.io.subtract := 1.U
      result := core.io.result
    }
    is (Ops.sbc) {
      core.io.subtract := 1.U
      core.io.carryIn := flagsIn.carry
      result := core.io.result
    }
    is (Ops.and) {
      result := io.a & io.b
    }
    is (Ops.xor) {
      result := io.a ^ io.b
    }
    is (Ops.or) {
      result := io.a | io.b
    }
    is (Ops.cp) {
      core.io.subtract := 1.U
      result := core.io.result
    }
  }

  // set flags
  flagsOut.zero := result === 0.U
  flagsOut.half := core.io.halfCarryOut
  flagsOut.subtract := core.io.subtract
  flagsOut.carry := core.io.carryOut

  // set outputs
  io.result := result
  io.flagsOut := flagsOut.asUInt()
}
