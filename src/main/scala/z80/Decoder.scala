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

object Reg8 {
  val A = 0.U; val F = 1.U
  val B = 2.U; val C = 3.U
  val D = 4.U; val E = 5.U
  val H = 6.U; val L = 7.U
}

object Reg16 {
  val BC = 0.U
  val DE = 1.U
  val HL = 2.U
}

case class Microcode(op: UInt, a: Option[UInt], b: Option[UInt])

object Decoder {
  val instructions = Seq(
    // NOP
    (0x00.U, Microcode(Ops.add, None, None)),
    // INC A
    (0x3c.U, Microcode(Ops.add, Some(Reg8.A), Some(Reg8.A))),
    // INC B
    (0x04.U, Microcode(Ops.add, Some(Reg8.B), Some(Reg8.B))),
    // INC C
    (0x0c.U, Microcode(Ops.add, Some(Reg8.C), Some(Reg8.C))),
    // INC D
    (0x14.U, Microcode(Ops.add, Some(Reg8.D), Some(Reg8.D))),
  )
}

class Decoder extends Module {
  val io = IO(new Bundle {
    val ir = Input(UInt(8.W))
    val op = Output(UInt(5.W))
    val a = Output(UInt(4.W))
    val b = Output(UInt(4.W))
  })

  def decodeMicrocode(microcode: Microcode) = {
    io.op := microcode.op
    microcode.a match {
      case Some(i) => {
        io.a := i
      }
      case None => {}
    }
    microcode.b match {
      case Some(i) => {
        io.b := i
      }
      case None => {}
    }
  }

  io.op := 0.U
  io.a := 0.U
  io.b := 0.U

  for (instruction <- Decoder.instructions) {
    val code = instruction._1
    val microcode = instruction._2

    when (io.ir === code) {
      decodeMicrocode(microcode)
    }
  }
}
