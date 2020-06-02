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

class CPUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CPU"

  it should "assert M1 during T1 and T2" in {
    test(new CPU) { c =>
      c.io.m1.expect(true.B) // T1
      c.clock.step()
      c.io.m1.expect(true.B) // T2
      c.clock.step()
      c.io.m1.expect(false.B) // T3
      c.clock.step()
      c.io.m1.expect(false.B) // T4
    }
  }

  it should "fetch an instruction during T2" in {
    test(new CPU) { c =>
      c.io.mreq.expect(false.B)
      c.io.rd.expect(false.B)
      c.clock.step()
      c.io.mreq.expect(true.B)
      c.io.rd.expect(true.B)
      c.clock.step(cycles = 2)
      c.io.mreq.expect(false.B)
      c.io.rd.expect(false.B)
    }
  }

  it should "increment the program counter every four clock cycles" in {
    test(new CPU) { c =>
      c.io.addr.expect(0x00.U)
      c.clock.step(4)
      c.io.addr.expect(0x01.U)
    }
  }

  it should "execute a INC A instruction" in {
    test(new CPU) { c =>
      c.io.din.poke(Instructions.INC_A.U)
      c.clock.step(4)
      c.io.registers8(Reg8.A).expect(0x01.U)
    }
  }

  it should "execute a INC B instruction" in {
    test(new CPU) { c =>
      c.io.din.poke(Instructions.INC_B.U)
      c.clock.step(4)
      c.io.registers8(Reg8.B).expect(0x01.U)
    }
  }
}
