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

class ALUTest extends FunSpec with ChiselScalatestTester with Matchers {
  // ALU I/O value object
  case class Value(a: UInt, b: UInt, flagsIn: UInt, result: UInt, flagsOut: UInt)

  def testALU(op: UInt, value: Value, c: ALU) = {
    c.io.op.poke(op)
    c.io.a.poke(value.a)
    c.io.b.poke(value.b)
    c.io.flagsIn.poke(value.flagsIn)
    c.io.result.expect(value.result)
    c.io.flagsOut.expect(value.flagsOut)
  }

  describe("ADD") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 0.U, "b0100_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(64.U, 64.U, "b0000_0000".U, 128.U, "b1000_0100".U),
      Value(255.U, 1.U, "b0000_0000".U, 0.U, "b0101_0001".U),
    )

    for (value <- values) {
      it(s"should add ${value.a.litValue()} and ${value.b.litValue()}") {
        test(new ALU) { testALU(Ops.add, value, _) }
      }
    }
  }

  describe("ADC") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 1.U, "b0000_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(64.U, 64.U, "b0000_0000".U, 128.U, "b1000_0100".U),
      Value(255.U, 1.U, "b0000_0000".U, 0.U, "b0101_0001".U),
    )

    for (value <- values) {
      it(s"should add ${value.a.litValue()} and ${value.b.litValue()} (with carry)") {
        test(new ALU) { testALU(Ops.adc, value, _) }
      }
    }
  }

  describe("SUB") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 0.U, "b0100_0010".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0010".U),
      Value(0.U, 1.U, "b0000_0000".U, 255.U, "b1001_0111".U),
      Value(128.U, 128.U, "b0000_0000".U, 0.U, "b0100_0110".U),
      Value(255.U, 1.U, "b0000_0000".U, 254.U, "b1000_0010".U),
    )

    for (value <- values) {
      it(s"should subtract ${value.a.litValue()} and ${value.b.litValue()}") {
        test(new ALU) { testALU(Ops.sub, value, _) }
      }
    }
  }

  describe("SBC") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 255.U, "b1001_0111".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0010".U),
      Value(0.U, 1.U, "b0000_0000".U, 255.U, "b1001_0111".U),
      Value(128.U, 128.U, "b0000_0000".U, 0.U, "b0100_0110".U),
      Value(255.U, 1.U, "b0000_0000".U, 254.U, "b1000_0010".U),
    )

    for (value <- values) {
      it(s"should subtract ${value.a.litValue()} and ${value.b.litValue()} (with carry)") {
        test(new ALU) { testALU(Ops.sbc, value, _) }
      }
    }
  }

  describe("CP") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 0.U, "b0100_0010".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0010".U),
      Value(0.U, 1.U, "b0000_0000".U, 255.U, "b1001_0111".U),
      Value(128.U, 128.U, "b0000_0000".U, 0.U, "b0100_0110".U),
      Value(255.U, 1.U, "b0000_0000".U, 254.U, "b1000_0010".U),
    )

    for (value <- values) {
      it(s"should compare ${value.a.litValue()} and ${value.b.litValue()}") {
        test(new ALU) { testALU(Ops.cp, value, _) }
      }
    }
  }

  describe("AND") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(1.U, 1.U, "b0000_0000".U, 1.U, "b0001_0000".U),
      Value(255.U, 0.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(0.U, 255.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(255.U, 255.U, "b0000_0000".U, 255.U, "b1001_0100".U),
    )

    for (value <- values) {
      it(s"should AND ${value.a.litValue()} and ${value.b.litValue()}") {
        test(new ALU) { testALU(Ops.and, value, _) }
      }
    }
  }

  describe("OR") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(1.U, 1.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(255.U, 0.U, "b0000_0000".U, 255.U, "b1000_0100".U),
      Value(0.U, 255.U, "b0000_0000".U, 255.U, "b1000_0100".U),
      Value(255.U, 255.U, "b0000_0000".U, 255.U, "b1000_0100".U),
    )

    for (value <- values) {
      it(s"should OR ${value.a.litValue()} and ${value.b.litValue()}") {
        test(new ALU) { testALU(Ops.or, value, _) }
      }
    }
  }

  describe("XOR") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(1.U, 1.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(255.U, 0.U, "b0000_0000".U, 255.U, "b1000_0100".U),
      Value(0.U, 255.U, "b0000_0000".U, 255.U, "b1000_0100".U),
      Value(255.U, 255.U, "b0000_0000".U, 0.U, "b0100_0100".U),
    )

    for (value <- values) {
      it(s"should XOR ${value.a.litValue()} and ${value.b.litValue()}") {
        test(new ALU) { testALU(Ops.xor, value, _) }
      }
    }
  }

  describe("BIT") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 0.U, "b0101_0101".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0001_0000".U),
      Value(0.U, 1.U, "b0000_0001".U, 0.U, "b0101_0101".U),
      Value(1.U, 1.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(128.U, 7.U, "b0000_0000".U, 128.U, "b1001_0000".U),
    )

    for (value <- values) {
      it(s"should test bit ${value.b.litValue()} in ${value.a.litValue()}") {
        test(new ALU) { testALU(Ops.bit, value, _) }
      }
    }
  }

  describe("SET") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 1.U, "b0000_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 2.U, "b0000_0000".U),
      Value(1.U, 1.U, "b0000_0000".U, 3.U, "b0000_0000".U),
      Value(128.U, 7.U, "b0000_0000".U, 128.U, "b0000_0000".U),
    )

    for (value <- values) {
      it(s"should set bit ${value.b.litValue()} of ${value.a.litValue()}") {
        test(new ALU) { testALU(Ops.set, value, _) }
      }
    }
  }

  describe("RL") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0001".U, 3.U, "b0000_0100".U),
      Value(64.U, 0.U, "b0000_0000".U, 128.U, "b1000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 0.U, "b0100_0101".U),
    )

    for (value <- values) {
      it(s"should rotate ${value.a.litValue()} to the left") {
        test(new ALU) { testALU(Ops.rl, value, _) }
      }
    }
  }

  describe("RLC") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0001".U, 2.U, "b0000_0000".U),
      Value(64.U, 0.U, "b0000_0000".U, 128.U, "b1000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 1.U, "b0000_0001".U),
    )

    for (value <- values) {
      it(s"should rotate ${value.a.litValue()} to the left (circular)") {
        test(new ALU) { testALU(Ops.rlc, value, _) }
      }
    }
  }

  describe("RR") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0001".U, 128.U, "b1000_0001".U),
      Value(64.U, 0.U, "b0000_0000".U, 32.U, "b0000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 64.U, "b0000_0000".U),
    )

    for (value <- values) {
      it(s"should rotate ${value.a.litValue()} to the right") {
        test(new ALU) { testALU(Ops.rr, value, _) }
      }
    }
  }

  describe("RRC") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 128.U, "b1000_0001".U),
      Value(64.U, 0.U, "b0000_0000".U, 32.U, "b0000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 64.U, "b0000_0000".U),
    )

    for (value <- values) {
      it(s"should rotate ${value.a.litValue()} to the right (circular)") {
        test(new ALU) { testALU(Ops.rrc, value, _) }
      }
    }
  }

  describe("SLA") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 2.U, "b0000_0000".U),
      Value(64.U, 0.U, "b0000_0000".U, 128.U, "b1000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 0.U, "b0100_0101".U),
    )

    for (value <- values) {
      it(s"should shift ${value.a.litValue()} to the left (arithmetic)") {
        test(new ALU) { testALU(Ops.sla, value, _) }
      }
    }
  }

  describe("SLL") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 3.U, "b0000_0100".U),
      Value(64.U, 0.U, "b0000_0000".U, 129.U, "b1000_0100".U),
      Value(128.U, 0.U, "b0000_0000".U, 1.U, "b0000_0001".U),
    )

    for (value <- values) {
      it(s"should shift ${value.a.litValue()} to the left (logical)") {
        test(new ALU) { testALU(Ops.sll, value, _) }
      }
    }
  }

  describe("SRA") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 0.U, "b0100_0101".U),
      Value(64.U, 0.U, "b0000_0000".U, 32.U, "b0000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 192.U, "b1000_0100".U),
    )

    for (value <- values) {
      it(s"should shift ${value.a.litValue()} to the right (arithmetic)") {
        test(new ALU) { testALU(Ops.sra, value, _) }
      }
    }
  }

  describe("SRL") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 0.U, "b0100_0101".U),
      Value(64.U, 0.U, "b0000_0000".U, 32.U, "b0000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 64.U, "b0000_0000".U),
    )

    for (value <- values) {
      it(s"should shift ${value.a.litValue()} to the right (logical)") {
        test(new ALU) { testALU(Ops.srl, value, _) }
      }
    }
  }

  describe("RLD") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(48.U, 16.U, "b0000_0000".U, 49.U, "b0000_0000".U),
      Value(144.U, 16.U, "b0000_0000".U, 145.U, "b1000_0000".U),
    )

    for (value <- values) {
      it(s"should rotate upper BCD digit between ${value.a.litValue()} and ${value.b.litValue()}") {
        test(new ALU) { testALU(Ops.rld, value, _) }
      }
    }
  }

  describe("RRD") {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(48.U, 1.U, "b0000_0000".U, 49.U, "b0000_0000".U),
      Value(144.U, 1.U, "b0000_0000".U, 145.U, "b1000_0000".U),
    )

    for (value <- values) {
      it(s"should rotate lower BCD digit between ${value.a.litValue()} and ${value.b.litValue()}") {
        test(new ALU) { testALU(Ops.rrd, value, _) }
      }
    }
  }
}
