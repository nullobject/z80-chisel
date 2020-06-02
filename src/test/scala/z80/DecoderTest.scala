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
 * Copyright (dut) 2020 Josh Bassett
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
import chiseltest._
import org.scalatest._

class DecoderTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "instructions"

  it should "NOP" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.NOP.U)
      dut.io.op.expect(Ops.NOP.U)
      dut.io.busIndex.expect(0.U)
    }
  }

  it should "INC A" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.INC_A.U)
      dut.io.op.expect(Ops.INC.U)
      dut.io.busIndex.expect(Reg8.A.U)
    }
  }

  it should "INC B" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.INC_B.U)
      dut.io.op.expect(Ops.INC.U)
      dut.io.busIndex.expect(Reg8.B.U)
    }
  }

  it should "LD A" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.LD_A.U)
      dut.io.mCycle.poke(0.U)
      dut.io.op.expect(Ops.NOP.U)
      dut.io.busIndex.expect(0.U)
    }
  }

  it should "HALT" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.HALT.U)
      dut.io.mCycle.poke(0.U)
      dut.io.op.expect(Ops.NOP.U)
      dut.io.busIndex.expect(0.U)
      dut.io.halt.expect(true.B)
    }
  }
}
