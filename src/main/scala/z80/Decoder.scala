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

case class Microcode(op: UInt, a: Option[UInt], b: Option[UInt])

object Decoder {
  import Instructions._

  val instructions = Seq(
    (NOP   -> Microcode(Ops.add, None, None)),
    (INC_A -> Microcode(Ops.add, Some(Reg8.A), Some(Reg8.A))),
    (INC_B -> Microcode(Ops.add, Some(Reg8.B), Some(Reg8.B))),
  )
}

/**
 * Decodes the instruction register value into an operation and address bus indexes.
 */
class Decoder extends Module {
  val io = IO(new Bundle {
    val ir = Input(UInt(8.W))
    val op = Output(UInt(5.W))
    val indexA = Output(UInt(4.W))
    val indexB = Output(UInt(4.W))
  })

  /**
   * Decodes the given microcode and sets the module outputs.
   */
  private def decodeMicrocode(microcode: Microcode) = {
    io.op := microcode.op
    microcode.a match {
      case Some(i) => {
        io.indexA := i
      }
      case None => {}
    }
    microcode.b match {
      case Some(i) => {
        io.indexB := i
      }
      case None => {}
    }
  }

  // default outputs
  io.op := 0.U
  io.indexA := 0.U
  io.indexB := 0.U

  // decode the instructions
  for (instruction <- Decoder.instructions) {
    val code = instruction._1
    val microcode = instruction._2

    when (io.ir === code) {
      decodeMicrocode(microcode)
    }
  }
}
