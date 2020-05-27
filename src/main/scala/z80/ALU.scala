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
  val add :: adc :: sub :: sbc :: cp :: and :: xor :: or :: rl :: rlc :: rr :: rrc :: sla :: sll :: sra :: srl :: rld :: rrd :: Nil = Enum(18)
}

/**
 * Processor flags
 */
class Flags extends Bundle {
  val sign = UInt(1.W)
  val zero = UInt(1.W)
  val unused2 = UInt(1.W)
  val halfCarry = UInt(1.W)
  val unused1 = UInt(1.W)
  val overflow = UInt(1.W)
  val subtract = UInt(1.W)
  val carry = UInt(1.W)
}

/**
 * Arithmetic logic unit
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
  flagsOut.sign := result(7)
  flagsOut.zero := result === 0.U
  flagsOut.halfCarry := core.io.halfCarryOut
  flagsOut.overflow := (io.a(7) && io.b(7) && !result(7)) || (!io.a(7) && !io.b(7) && result(7))
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
    is (Ops.cp) {
      core.io.subtract := 1.U
      result := core.io.result
    }
    is (Ops.and) {
      result := io.a & io.b
      flagsOut.carry := 0.U
      flagsOut.halfCarry := 1.U
    }
    is (Ops.xor) {
      result := io.a ^ io.b
      flagsOut.carry := 0.U
      flagsOut.halfCarry := 0.U
    }
    is (Ops.or) {
      result := io.a | io.b
      flagsOut.carry := 0.U
      flagsOut.halfCarry := 0.U
    }
    is (Ops.rl) {
      result := Cat(io.a(6, 0), flagsIn.carry)
      flagsOut.carry := io.a(7)
      flagsOut.halfCarry := 0.U
    }
    is (Ops.rlc) {
      result := Cat(io.a(6, 0), io.a(7))
      flagsOut.carry := io.a(7)
      flagsOut.halfCarry := 0.U
    }
    is (Ops.rr) {
      result := Cat(flagsIn.carry, io.a(7, 1))
      flagsOut.carry := io.a(0)
      flagsOut.halfCarry := 0.U
    }
    is (Ops.rrc) {
      result := Cat(io.a(0), io.a(7, 1))
      flagsOut.carry := io.a(0)
      flagsOut.halfCarry := 0.U
    }
    is (Ops.sla) {
      result := Cat(io.a(6, 0), 0.U)
      flagsOut.carry := io.a(7)
      flagsOut.halfCarry := 0.U
    }
    is (Ops.sll) {
      result := Cat(io.a(6, 0), 1.U)
      flagsOut.carry := io.a(7)
      flagsOut.halfCarry := 0.U
    }
    is (Ops.sra) {
      result := Cat(io.a(7), io.a(7, 1))
      flagsOut.carry := io.a(0)
      flagsOut.halfCarry := 0.U
    }
    is (Ops.srl) {
      result := Cat(0.U, io.a(7, 1))
      flagsOut.carry := io.a(0)
      flagsOut.halfCarry := 0.U
    }
    is (Ops.rld) {
      result := Cat(io.a(7, 4), io.b(7, 4))
      flagsOut.halfCarry := 0.U
    }
    is (Ops.rrd) {
      result := Cat(io.a(7, 4), io.b(3, 0))
      flagsOut.halfCarry := 0.U
    }
  }

  // set outputs
  io.result := result
  io.flagsOut := flagsOut.asUInt()
}
