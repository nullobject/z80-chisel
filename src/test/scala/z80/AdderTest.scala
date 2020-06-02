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

class AdderTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "An add operation"

  it should "add the inputs (without carry)" in {
    test(new Adder) { dut =>
      dut.io.a.poke(3.U)
      dut.io.b.poke(2.U)
      dut.io.result.expect(5.U)
      dut.io.halfCarryOut.expect(false.B)
      dut.io.carryOut.expect(false.B)
    }
  }

  it should "add the inputs (with carry)" in {
    test(new Adder) { dut =>
      dut.io.a.poke(3.U)
      dut.io.b.poke(2.U)
      dut.io.carryIn.poke(true.B)
      dut.io.result.expect(6.U)
      dut.io.halfCarryOut.expect(false.B)
      dut.io.carryOut.expect(false.B)
    }
  }

  it should "set the half-carry flag" in {
    test(new Adder) { dut =>
      dut.io.a.poke(15.U)
      dut.io.b.poke(1.U)
      dut.io.result.expect(16.U)
      dut.io.halfCarryOut.expect(true.B)
      dut.io.carryOut.expect(false.B)
    }
  }

  it should "set the carry flag" in {
    test(new Adder) { dut =>
      dut.io.a.poke(255.U)
      dut.io.b.poke(1.U)
      dut.io.result.expect(0.U)
      dut.io.halfCarryOut.expect(true.B)
      dut.io.carryOut.expect(true.B)
    }
  }

  behavior of "A subtract operation"

  it should "subtract the inputs (without carry)" in {
    test(new Adder) { dut =>
      dut.io.subtract.poke(true.B)
      dut.io.a.poke(3.U)
      dut.io.b.poke(2.U)
      dut.io.result.expect(1.U)
      dut.io.halfCarryOut.expect(false.B)
      dut.io.carryOut.expect(false.B)
    }
  }

  it should "subtract the inputs (with carry)" in {
    test(new Adder) { dut =>
      dut.io.subtract.poke(true.B)
      dut.io.a.poke(3.U)
      dut.io.b.poke(2.U)
      dut.io.carryIn.poke(true.B)
      dut.io.result.expect(0.U)
      dut.io.halfCarryOut.expect(false.B)
      dut.io.carryOut.expect(false.B)
    }
  }

  it should "set the half-carry flag" in {
    test(new Adder) { dut =>
      dut.io.subtract.poke(true.B)
      dut.io.a.poke(16.U)
      dut.io.b.poke(1.U)
      dut.io.result.expect(15.U)
      dut.io.halfCarryOut.expect(true.B)
      dut.io.carryOut.expect(false.B)
    }
  }

  it should "set the carry flag" in {
    test(new Adder) { dut =>
      dut.io.subtract.poke(true.B)
      dut.io.a.poke(0.U)
      dut.io.b.poke(1.U)
      dut.io.result.expect(255.U)
      dut.io.halfCarryOut.expect(true.B)
      dut.io.carryOut.expect(true.B)
    }
  }
}
