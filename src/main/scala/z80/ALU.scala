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

/**
 * Operations
 */
object Ops {
  val ADD = 0.U
  val ADC = 1.U
  val SUB = 2.U
  val SBC = 3.U
  val CP  = 4.U
  val AND = 5.U
  val OR  = 6.U
  val XOR = 7.U
  val INC = 8.U
  val DEC = 9.U
  val BIT = 10.U
  val SET = 11.U
  val RES = 12.U
  val RL  = 13.U
  val RLC = 14.U
  val RR  = 15.U
  val RRC = 16.U
  val SLA = 17.U
  val SLL = 18.U
  val SRA = 19.U
  val SRL = 20.U
  val RLD = 21.U
  val RRD = 22.U
}

/**
 * Processor flags
 */
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
 * The 8-bit arithmetic and logical instructions of the CPU are executed in the Arithmetic Logic Unit (ALU).
 */
class ALU extends Module {
  val io = IO(new Bundle {
    val op = Input(UInt(5.W))
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val flagsIn = Input(Bits(8.W))
    val result = Output(UInt(8.W))
    val flagsOut = Output(Bits(8.W))
  })

  /**
   * Calculates the bitmask for a given number.
   */
  private def bitmask(n: UInt) = (1.U << n).asUInt()

  val result = WireDefault(0.U(8.W))

  // flags
  val flagsIn = io.flagsIn.asTypeOf(new Flags)
  val flagsOut = Wire(new Flags)

  val overflow = (io.a(7) && io.b(7) && !result(7)) || (!io.a(7) && !io.b(7) && result(7))
  val parity = !result.xorR()

  // arithmetic core for addition/subtraction
  val adder = Module(new Adder)
  adder.io.subtract := io.op === Ops.SUB || io.op === Ops.SBC || io.op === Ops.CP || io.op === Ops.DEC
  adder.io.a := io.a
  adder.io.b := io.b
  adder.io.carryIn := flagsIn.carry && (io.op === Ops.ADC || io.op === Ops.SBC)

  // default flags
  flagsOut.sign := false.B
  flagsOut.zero := false.B
  flagsOut.unused2 := false.B
  flagsOut.halfCarry := false.B
  flagsOut.unused1 := false.B
  flagsOut.overflow := false.B
  flagsOut.subtract := adder.io.subtract
  flagsOut.carry := false.B

  switch (io.op) {
    is (Ops.ADD) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }
    is (Ops.ADC) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }
    is (Ops.SUB) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }
    is (Ops.SBC) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }
    is (Ops.CP) {
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := overflow
      flagsOut.carry := adder.io.carryOut
    }
    is (Ops.AND) {
      result := io.a & io.b
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := true.B
      flagsOut.overflow := parity
    }
    is (Ops.XOR) {
      result := io.a ^ io.b
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
    }
    is (Ops.OR) {
      result := io.a | io.b
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
    }
    is (Ops.INC) {
      adder.io.b := 1.U
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := io.a === 127.U
      flagsOut.carry := flagsIn.carry
    }
    is (Ops.DEC) {
      adder.io.b := 1.U
      result := adder.io.result
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := adder.io.halfCarryOut
      flagsOut.overflow := io.a === 128.U
      flagsOut.carry := flagsIn.carry
    }
    is (Ops.BIT) {
      result := io.a & bitmask(io.b(2, 0))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.halfCarry := true.B
      flagsOut.overflow := parity
      flagsOut.carry := flagsIn.carry
    }
    is (Ops.SET) {
      result := io.a | bitmask(io.b(2, 0))
    }
    is (Ops.RES) {
      result := io.a & (~bitmask(io.b(2, 0))).asUInt()
    }
    is (Ops.RL) {
      result := Cat(io.a(6, 0), flagsIn.carry)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(7)
    }
    is (Ops.RLC) {
      result := Cat(io.a(6, 0), io.a(7))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(7)
    }
    is (Ops.RR) {
      result := Cat(flagsIn.carry, io.a(7, 1))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(0)
    }
    is (Ops.RRC) {
      result := Cat(io.a(0), io.a(7, 1))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(0)
    }
    is (Ops.SLA) {
      result := Cat(io.a(6, 0), 0.U)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(7)
    }
    is (Ops.SLL) {
      result := Cat(io.a(6, 0), 1.U)
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(7)
    }
    is (Ops.SRA) {
      result := Cat(io.a(7), io.a(7, 1))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(0)
    }
    is (Ops.SRL) {
      result := Cat(0.U, io.a(7, 1))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
      flagsOut.carry := io.a(0)
    }
    is (Ops.RLD) {
      result := Cat(io.a(7, 4), io.b(7, 4))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
    }
    is (Ops.RRD) {
      result := Cat(io.a(7, 4), io.b(3, 0))
      flagsOut.zero := result === 0.U
      flagsOut.sign := result(7)
      flagsOut.overflow := parity
    }
  }

  // set outputs
  io.result := result
  io.flagsOut := flagsOut.asUInt()
}
