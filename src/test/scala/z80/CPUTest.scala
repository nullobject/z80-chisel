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

class CPUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "FSM"

  it should "assert M1 during T1 and T2" in {
    test(new CPU) { dut =>
      dut.io.m1.expect(true.B) // T1
      dut.clock.step()
      dut.io.m1.expect(true.B) // T2
      dut.clock.step()
      dut.io.m1.expect(false.B) // T3
      dut.clock.step()
      dut.io.m1.expect(false.B) // T4
    }
  }

  it should "fetch an instruction during T2" in {
    test(new CPU) { dut =>
      dut.io.mreq.expect(false.B)
      dut.io.rd.expect(false.B)
      dut.clock.step()
      dut.io.mreq.expect(true.B)
      dut.io.rd.expect(true.B)
      dut.clock.step()
      dut.io.mreq.expect(false.B)
      dut.io.rd.expect(false.B)
      dut.clock.step()
      dut.io.mreq.expect(false.B)
      dut.io.rd.expect(false.B)
    }
  }

  behavior of "program counter"

  it should "increment the program counter every four clock cycles" in {
    test(new CPU) { dut =>
      dut.io.addr.expect(0.U)
      dut.clock.step(4)
      dut.io.addr.expect(1.U)
      dut.clock.step(4)
      dut.io.addr.expect(2.U)
    }
  }

  "INC A" should "increment the A register" in {
    test(new CPU) { dut =>
      dut.io.din.poke(Instructions.INC_A.U)
      dut.io.debug.registers8(Reg8.A).expect(0.U)
      dut.clock.step(4)
      dut.io.debug.registers8(Reg8.A).expect(1.U)
    }
  }

  "INC B" should "increment the B register" in {
    test(new CPU) { dut =>
      dut.io.din.poke(Instructions.INC_B.U)
      dut.io.debug.registers8(Reg8.B).expect(0.U)
      dut.clock.step(4)
      dut.io.debug.registers8(Reg8.B).expect(1.U)
    }
  }

  "LD A" should "load a value into the A register" in {
    test(new CPU) { dut =>
      dut.io.din.poke(Instructions.LD_A.U)
      dut.clock.step(4)
      dut.io.din.poke(1.U)
      dut.io.debug.registers8(Reg8.A).expect(0.U)
      dut.clock.step(4)
      dut.io.debug.registers8(Reg8.A).expect(1.U)
    }
  }

  "HALT" should "halt the CPU" in {
    test(new CPU) { dut =>
      dut.io.din.poke(Instructions.HALT.U)
      dut.io.halt.expect(false.B)
      dut.clock.step(4)
      dut.io.halt.expect(true.B)
      dut.io.din.poke(Instructions.NOP.U)
      dut.clock.step(4)
      dut.io.halt.expect(true.B)
    }
  }
}
