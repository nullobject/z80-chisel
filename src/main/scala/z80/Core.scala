package z80

import chisel3._
import chisel3.util._

/**
 * This module adds or subtracts two numbers, with carry in and carry out bits.
 */
class Core extends Module {
  val io = IO(new Bundle {
    val subtract = Input(UInt(1.W))
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val carryIn = Input(UInt(1.W))
    val result = Output(UInt(8.W))
    val halfCarryOut= Output(UInt(1.W))
    val carryOut= Output(UInt(1.W))
  })

  val a = io.a
  val b = Mux(io.subtract.asBool(), ~io.b, io.b).asUInt()
  val x = (a(3, 0) +& b(3, 0)) + (io.carryIn ^ io.subtract)
  val halfCarry = x(4)
  val y = (a(7, 4) +& b(7, 4)) + halfCarry
  val carry = y(4)

  io.result := Cat(y(3, 0), x(3, 0))
  io.halfCarryOut := halfCarry ^ io.subtract
  io.carryOut := carry ^ io.subtract
}
