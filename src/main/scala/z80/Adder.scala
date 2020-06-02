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
 * Performs an addition or subtraction operation with two 8-bit values,
 * including carry and half-carry flags.
 */
class Adder extends Module {
  val io = IO(new Bundle {
    /** Assert the subtract flag to perform a subtract operation */
    val subtract = Input(Bool())
    /** Operand A */
    val a = Input(UInt(8.W))
    /** Operand B */
    val b = Input(UInt(8.W))
    /** Carry input */
    val carryIn = Input(Bool())
    /** The result of the addition/subtraction */
    val result = Output(UInt(8.W))
    /** Half-carry output */
    val halfCarryOut = Output(Bool())
    /** Carry output */
    val carryOut = Output(Bool())
  })

  // Invert the B operand for a subtract operation
  val b = Mux(io.subtract.asBool(), ~io.b, io.b).asUInt()

  // Low nibble
  val lowNibble = (io.a(3, 0) +& b(3, 0)) + (io.carryIn ^ io.subtract)
  val halfCarry = lowNibble(4)

  // High nibble
  val highNibble = (io.a(7, 4) +& b(7, 4)) + halfCarry
  val carry = highNibble(4)

  // Outputs
  io.result := highNibble(3, 0) ## lowNibble(3, 0)
  io.halfCarryOut := halfCarry ^ io.subtract
  io.carryOut := carry ^ io.subtract
}
