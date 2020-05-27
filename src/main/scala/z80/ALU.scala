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

  val core = Module(new Core)
  core.io.subtract := io.op === Ops.sub || io.op === Ops.sbc || io.op === Ops.cp
  core.io.a := io.a
  core.io.b := io.b
  core.io.carryIn := flagsIn.carry.asBool() && (io.op === Ops.adc || io.op === Ops.sbc)

  // set flags
  flagsOut.zero := core.io.result === 0.U
  flagsOut.half := core.io.halfCarryOut
  flagsOut.subtract := core.io.subtract
  flagsOut.carry := core.io.carryOut

  // set outputs
  io.result := core.io.result
  io.flagsOut := flagsOut.asUInt()
}
