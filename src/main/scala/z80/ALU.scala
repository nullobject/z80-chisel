/*
 *   __   __     __  __     __         __
 *  /\ "-.\ \   /\ \/\ \   /\ \       /\ \
 *  \ \ \-.  \  \ \ \_\ \  \ \ \____  \ \ \____
 *   \ \_\\"\_\  \ \_____\  \ \_____\  \ \_____\
 *    \/_/ \/_/   \/_____/   \/_____/   \/_____/
 *   ______     ______       __     ______     ______     ______
 *  /\  __ \   /\  == \     /\ \   /\  ___\   /\  ___\   /\__  _\
 *  \ \ \/\ \  \ \  __<    _\_\ \  \ \  __\   \ \ \____  \/_/\ \/
 *   \ \_____\  \ \_____\ /\_____\  \ \_____\  \ \_____\    \ \_\
 *    \/_____/   \/_____/ \/_____/   \/_____/   \/_____/     \/_/
 *
 * https://joshbassett.info
 * https://twitter.com/nullobject
 * https://github.com/nullobject
 *
 * Copyright (c) 2020 Josh Bassett
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package z80

import chisel3._
import chisel3.util._

/** Operations */
object Ops {
  val NOP = 0x00
  val ADD = 0x01
  val ADC = 0x02
  val SUB = 0x03
  val SBC = 0x04
  val CP  = 0x05
  val AND = 0x06
  val OR  = 0x07
  val XOR = 0x08
  val INC = 0x09
  val DEC = 0x0a
  val BIT = 0x0b
  val SET = 0x0c
  val RES = 0x0d
  val RL  = 0x0e
  val RLC = 0x0f
  val RR  = 0x10
  val RRC = 0x11
  val SLA = 0x12
  val SLL = 0x13
  val SRA = 0x14
  val SRL = 0x15
  val RLD = 0x16
  val RRD = 0x17
}

/** Processor flags */
class Flags extends Bundle {
  val sign = Bool()
  val zero = Bool()
  val unused2 = Bool()
  val halfCarry = Bool()
  val unused1 = Bool()
  val overflow = Bool()
  val subtract = Bool()
  val carry = Bool()
}

/**
 * The 8-bit arithmetic and logical instructions of the CPU are executed in the
 * Arithmetic Logic Unit (ALU).
 */
class ALU extends Module {
  val io = IO(new Bundle {
    /** Operation */
    val op = Input(UInt(5.W))
    /** Operand A */
    val a = Input(UInt(8.W))
    /** Operand B */
    val b = Input(UInt(8.W))
    /** Input flags */
    val flagsIn = Input(Bits(8.W))
    /** The result of the operation */
    val result = Output(UInt(8.W))
    /** Output flags */
    val flagsOut = Output(Bits(8.W))
  })

  /** Calculates the bitmask for a given number. */
  private def bitmask(n: UInt) = (1.U << n).asUInt()

  val result = WireDefault(0.U(8.W))

  // Flags
  val flagsIn = io.flagsIn.asTypeOf(new Flags)
  val flagsOut = Wire(new Flags)

  val overflow = (io.a(7) && io.b(7) && !result(7)) || (!io.a(7) && !io.b(7) && result(7))
  val parity = !result.xorR()

  // Arithmetic core for addition/subtraction
  val adder = Module(new Adder)
  adder.io.subtract := io.op === Ops.SUB.U || io.op === Ops.SBC.U || io.op === Ops.CP.U || io.op === Ops.DEC.U
  adder.io.a := io.a
  adder.io.b := io.b
  adder.io.carryIn := flagsIn.carry && (io.op === Ops.ADC.U || io.op === Ops.SBC.U)

  // Default flags
  flagsOut.sign := false.B
  flagsOut.zero := false.B
  flagsOut.unused2 := false.B
  flagsOut.halfCarry := false.B
  flagsOut.unused1 := false.B
  flagsOut.overflow := false.B
  flagsOut.subtract := adder.io.subtract
  flagsOut.carry := false.B

  switch (io.op) {
    is(Ops.ADD.U) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }

    is(Ops.ADC.U) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }

    is(Ops.SUB.U) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }

    is(Ops.SBC.U) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }

    is(Ops.CP.U) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }

    is(Ops.AND.U) {
      result := io.a & io.b
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := true.B
      flagsOut.overflow := parity
    }

    is(Ops.XOR.U) {
      result := io.a ^ io.b
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
    }

    is(Ops.OR.U) {
      result := io.a | io.b
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
    }

    is(Ops.INC.U) {
      adder.io.b := 1.U
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := io.a === 127.U
      flagsOut.carry := flagsIn.carry
    }

    is(Ops.DEC.U) {
      adder.io.b := 1.U
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := io.a === 128.U
      flagsOut.carry := flagsIn.carry
    }

    is(Ops.BIT.U) {
      result := io.a & bitmask(io.b(2, 0))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := true.B
      flagsOut.overflow := parity
      flagsOut.carry := flagsIn.carry
    }

    is(Ops.SET.U) {
      result := io.a | bitmask(io.b(2, 0))
    }

    is(Ops.RES.U) {
      result := io.a & (~bitmask(io.b(2, 0))).asUInt()
    }

    is(Ops.RL.U) {
      result := io.a(6, 0) ## flagsIn.carry
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(7)
    }

    is(Ops.RLC.U) {
      result := io.a(6, 0) ## io.a(7)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(7)
    }

    is(Ops.RR.U) {
      result := flagsIn.carry ## io.a(7, 1)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(0)
    }

    is(Ops.RRC.U) {
      result := io.a(0) ## io.a(7, 1)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(0)
    }

    is(Ops.SLA.U) {
      result := io.a(6, 0) ## 0.U
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(7)
    }

    is(Ops.SLL.U) {
      result := io.a(6, 0) ## 1.U
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(7)
    }

    is(Ops.SRA.U) {
      result := io.a(7) ## io.a(7, 1)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(0)
    }

    is(Ops.SRL.U) {
      result := 0.U ## io.a(7, 1)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(0)
    }

    is(Ops.RLD.U) {
      result := io.a(7, 4) ## io.b(7, 4)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
    }

    is(Ops.RRD.U) {
      result := io.a(7, 4) ## io.b(3, 0)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
    }
  }

  // Outputs
  io.result := result
  io.flagsOut := flagsOut.asUInt()
}
