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

class ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
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

  "ADD" should "add the operands" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 0.U, "b0100_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(64.U, 64.U, "b0000_0000".U, 128.U, "b1000_0100".U),
      Value(255.U, 1.U, "b0000_0000".U, 0.U, "b0101_0001".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.add, value, _) }
    }
  }

  "ADC" should "add the operands with carry" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 1.U, "b0000_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(64.U, 64.U, "b0000_0000".U, 128.U, "b1000_0100".U),
      Value(255.U, 1.U, "b0000_0000".U, 0.U, "b0101_0001".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.adc, value, _) }
    }
  }

  "SUB" should "subtract the operands" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 0.U, "b0100_0010".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0010".U),
      Value(0.U, 1.U, "b0000_0000".U, 255.U, "b1001_0111".U),
      Value(128.U, 128.U, "b0000_0000".U, 0.U, "b0100_0110".U),
      Value(255.U, 1.U, "b0000_0000".U, 254.U, "b1000_0010".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.sub, value, _) }
    }
  }

  "SBC" should "subtract the operands with carry" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 255.U, "b1001_0111".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0010".U),
      Value(0.U, 1.U, "b0000_0000".U, 255.U, "b1001_0111".U),
      Value(128.U, 128.U, "b0000_0000".U, 0.U, "b0100_0110".U),
      Value(255.U, 1.U, "b0000_0000".U, 254.U, "b1000_0010".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.sbc, value, _) }
    }
  }

  "CP" should "compare the operands" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0001".U, 0.U, "b0100_0010".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0010".U),
      Value(0.U, 1.U, "b0000_0000".U, 255.U, "b1001_0111".U),
      Value(128.U, 128.U, "b0000_0000".U, 0.U, "b0100_0110".U),
      Value(255.U, 1.U, "b0000_0000".U, 254.U, "b1000_0010".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.cp, value, _) }
    }
  }

  "AND" should "logical AND the operands" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(1.U, 1.U, "b0000_0000".U, 1.U, "b0001_0000".U),
      Value(255.U, 0.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(0.U, 255.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(255.U, 255.U, "b0000_0000".U, 255.U, "b1001_0100".U),
    )

    for (value <- values) {
        test(new ALU) { testALU(Ops.and, value, _) }
    }
  }

  "OR" should "logical OR the operands" in {
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
      test(new ALU) { testALU(Ops.or, value, _) }
    }
  }

  "XOR" should "logical XOR the operands" in {
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
      test(new ALU) { testALU(Ops.xor, value, _) }
    }
  }

  "INC" should "increment the operand" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 0.U, "b0000_0001".U, 1.U, "b0000_0001".U),
      Value(1.U, 0.U, "b0000_0000".U, 2.U, "b0000_0000".U),
      Value(127.U, 0.U, "b0000_0000".U, 128.U, "b1001_0100".U),
      Value(255.U, 1.U, "b0000_0000".U, 0.U, "b0101_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.inc, value, _) }
    }
  }

  "DEC" should "decrement the operand" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 255.U, "b1001_0010".U),
      Value(0.U, 0.U, "b0000_0001".U, 255.U, "b1001_0011".U),
      Value(1.U, 0.U, "b0000_0000".U, 0.U, "b0100_0010".U),
      Value(128.U, 0.U, "b0000_0000".U, 127.U, "b0001_0110".U),
      Value(255.U, 1.U, "b0000_0000".U, 254.U, "b1000_0010".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.dec, value, _) }
    }
  }

  "BIT" should "test whether a bit is set" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0001_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 0.U, "b0101_0100".U),
      Value(128.U, 7.U, "b0000_0000".U, 128.U, "b1001_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.bit, value, _) }
    }
  }

  "SET" should "set a bit" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 2.U, "b0000_0000".U),
      Value(128.U, 7.U, "b0000_0000".U, 128.U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.set, value, _) }
    }
  }

  "RES" should "reset a bit" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0000_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 0.U, "b0000_0000".U),
      Value(0.U, 1.U, "b0000_0000".U, 0.U, "b0000_0000".U),
      Value(255.U, 7.U, "b0000_0000".U, 127.U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.res, value, _) }
    }
  }

  "RL" should "rotate the bits to the left" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0001".U, 3.U, "b0000_0100".U),
      Value(64.U, 0.U, "b0000_0000".U, 128.U, "b1000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 0.U, "b0100_0101".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.rl, value, _) }
    }
  }

  "RLC" should "rotate the bits to the left (circular)" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0001".U, 2.U, "b0000_0000".U),
      Value(64.U, 0.U, "b0000_0000".U, 128.U, "b1000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 1.U, "b0000_0001".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.rlc, value, _) }
    }
  }

  "RR" should "rotate the bits to the right" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0001".U, 128.U, "b1000_0001".U),
      Value(64.U, 0.U, "b0000_0000".U, 32.U, "b0000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 64.U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.rr, value, _) }
    }
  }

  "RRC" should "rotate the bits to the right (circular)" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 128.U, "b1000_0001".U),
      Value(64.U, 0.U, "b0000_0000".U, 32.U, "b0000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 64.U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.rrc, value, _) }
    }
  }

  "SLA" should "shift the bits to the left (arithmetic)" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 2.U, "b0000_0000".U),
      Value(64.U, 0.U, "b0000_0000".U, 128.U, "b1000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 0.U, "b0100_0101".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.sla, value, _) }
    }
  }

  "SLL" should "shift the bits to the left (logical)" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 1.U, "b0000_0000".U),
      Value(1.U, 0.U, "b0000_0000".U, 3.U, "b0000_0100".U),
      Value(64.U, 0.U, "b0000_0000".U, 129.U, "b1000_0100".U),
      Value(128.U, 0.U, "b0000_0000".U, 1.U, "b0000_0001".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.sll, value, _) }
    }
  }

  "SRA" should "shift the bits to the right (arithmetic)" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 0.U, "b0100_0101".U),
      Value(64.U, 0.U, "b0000_0000".U, 32.U, "b0000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 192.U, "b1000_0100".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.sra, value, _) }
    }
  }

  "SRL" should "shift the bits to the right (logical)" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(1.U, 0.U, "b0000_0000".U, 0.U, "b0100_0101".U),
      Value(64.U, 0.U, "b0000_0000".U, 32.U, "b0000_0000".U),
      Value(128.U, 0.U, "b0000_0000".U, 64.U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.srl, value, _) }
    }
  }

  "RLD" should "rotate the upper BCD digit" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(48.U, 16.U, "b0000_0000".U, 49.U, "b0000_0000".U),
      Value(144.U, 16.U, "b0000_0000".U, 145.U, "b1000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.rld, value, _) }
    }
  }

  "RRD" should "rotate the lower BCD digit" in {
    val values = Seq(
      Value(0.U, 0.U, "b0000_0000".U, 0.U, "b0100_0100".U),
      Value(48.U, 1.U, "b0000_0000".U, 49.U, "b0000_0000".U),
      Value(144.U, 1.U, "b0000_0000".U, 145.U, "b1000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.rrd, value, _) }
    }
  }
}
