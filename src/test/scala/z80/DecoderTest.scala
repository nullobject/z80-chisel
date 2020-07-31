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
import chiseltest._
import org.scalatest._

class DecoderTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "instructions"

  "NOP" should "select NOP operation" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.NOP.U)
      dut.io.op.expect(Ops.NOP.U)
    }
  }

  "INC A" should "select INC operation" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.INC_A.U)
      dut.io.op.expect(Ops.INC.U)
    }
  }

  it should "write to the bus" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.INC_A.U)
      dut.io.destIndex.expect(Reg8.A.U)
      dut.io.store.expect(true.B)
    }
  }

  "INC B" should "select INC operation" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.INC_B.U)
      dut.io.op.expect(Ops.INC.U)
    }
  }

  it should "write to the bus" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.INC_B.U)
      dut.io.destIndex.expect(Reg8.B.U)
      dut.io.store.expect(true.B)
    }
  }

  "LD A" should "select NOP operation" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.LD_A.U)
      dut.io.op.expect(Ops.NOP.U)
    }
  }

  it should "write to the bus" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.LD_A.U)
      dut.io.mCycle.poke(1.U)
      dut.io.destIndex.expect(Reg8.A.U)
      dut.io.store.expect(true.B)
    }
  }

  "LD B" should "select NOP operation" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.LD_B.U)
      dut.io.op.expect(Ops.NOP.U)
    }
  }

  it should "write to the bus" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.LD_B.U)
      dut.io.mCycle.poke(1.U)
      dut.io.op.expect(Ops.NOP.U)
      dut.io.destIndex.expect(Reg8.B.U)
      dut.io.store.expect(true.B)
    }
  }

  "ADD A" should "select ADD operation" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.ADD_A.U)
      dut.io.op.expect(Ops.ADD.U)
    }
  }

  it should "write to the bus" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.ADD_A.U)
      dut.io.srcIndex.expect(Reg8.A.U)
      dut.io.destIndex.expect(Reg8.A.U)
      dut.io.store.expect(true.B)
    }
  }

  "ADD B" should "select ADD operation" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.ADD_B.U)
      dut.io.op.expect(Ops.ADD.U)
    }
  }

  it should "write to the bus" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.ADD_B.U)
      dut.io.srcIndex.expect(Reg8.B.U)
      dut.io.destIndex.expect(Reg8.A.U)
      dut.io.store.expect(true.B)
    }
  }

  it should "HALT" in {
    test(new Decoder) { dut =>
      dut.io.instruction.poke(Instructions.HALT.U)
      dut.io.op.expect(Ops.NOP.U)
      dut.io.halt.expect(true.B)
    }
  }
}
