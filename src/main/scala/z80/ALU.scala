package z80

import chisel3._
import chisel3.util._

/**
 * Operations
 */
object Ops {
  val add :: adc :: sub :: sbc :: and :: xor :: or :: cp :: rl :: rlc :: rr :: rrc :: sla :: sll :: sra :: srl :: Nil = Enum(16)
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

  // set flags
  flagsOut.zero := result === 0.U
  flagsOut.half := core.io.halfCarryOut
  flagsOut.subtract := core.io.subtract
  flagsOut.carry := core.io.carryOut

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
    is (Ops.rl) {
      result := Cat(io.a(6, 0), flagsIn.carry)
      flagsOut.carry := io.a(7)
      flagsOut.half := 0.U
    }
    is (Ops.rlc) {
      result := Cat(io.a(6, 0), io.a(7))
      flagsOut.carry := io.a(7)
      flagsOut.half := 0.U
    }
    is (Ops.rr) {
      result := Cat(flagsIn.carry, io.a(7, 1))
      flagsOut.carry := io.a(0)
      flagsOut.half := 0.U
    }
    is (Ops.rrc) {
      result := Cat(io.a(0), io.a(7, 1))
      flagsOut.carry := io.a(0)
      flagsOut.half := 0.U
    }
    is (Ops.sla) {
      result := Cat(io.a(6, 0), 0.U)
      flagsOut.carry := io.a(7)
      flagsOut.half := 0.U
    }
    is (Ops.sll) {
      result := Cat(io.a(6, 0), 1.U)
      flagsOut.carry := io.a(7)
      flagsOut.half := 0.U
    }
    is (Ops.sra) {
      result := Cat(io.a(7), io.a(7, 1))
      flagsOut.carry := io.a(0)
      flagsOut.half := 0.U
    }
    is (Ops.srl) {
      result := Cat(0.U, io.a(7, 1))
      flagsOut.carry := io.a(0)
      flagsOut.half := 0.U
    }
  }

  // set outputs
  io.result := result
  io.flagsOut := flagsOut.asUInt()
}
