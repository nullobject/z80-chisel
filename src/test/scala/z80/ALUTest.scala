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

class ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  // ALU I/O value object
  case class Value(a: UInt, b: UInt, result: UInt, flagsIn: UInt, flagsOut: UInt)

  def testALU(op: UInt, value: Value, alu: ALU) = {
    alu.io.op.poke(op)
    alu.io.a.poke(value.a)
    alu.io.b.poke(value.b)
    alu.io.flagsIn.poke(value.flagsIn)
    alu.io.result.expect(value.result)
    alu.io.flagsOut.expect(value.flagsOut)
  }

  "NOP" should "do nothing" in {
    val values = Seq(
      Value(1.U, 0.U, 0.U, "b0000_0000".U, "b0000_0000".U),
      Value(0.U, 1.U, 0.U, "b0000_0000".U, "b0000_0000".U)
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.NOP.U, value, _) }
    }
  }

  "ADD" should "add the operands" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0001".U, "b0100_0000".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(0.U, 1.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(64.U, 64.U, 128.U, "b0000_0000".U, "b1000_0100".U),
      Value(255.U, 1.U, 0.U, "b0000_0000".U, "b0101_0001".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.ADD.U, value, _) }
    }
  }

  "ADC" should "add the operands with carry" in {
    val values = Seq(
      Value(0.U, 0.U, 1.U, "b0000_0001".U, "b0000_0000".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(0.U, 1.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(64.U, 64.U, 128.U, "b0000_0000".U, "b1000_0100".U),
      Value(255.U, 1.U, 0.U, "b0000_0000".U, "b0101_0001".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.ADC.U, value, _) }
    }
  }

  "SUB" should "subtract the operands" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0001".U, "b0100_0010".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0000_0010".U),
      Value(0.U, 1.U, 255.U, "b0000_0000".U, "b1001_0111".U),
      Value(128.U, 128.U, 0.U, "b0000_0000".U, "b0100_0110".U),
      Value(255.U, 1.U, 254.U, "b0000_0000".U, "b1000_0010".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.SUB.U, value, _) }
    }
  }

  "SBC" should "subtract the operands with carry" in {
    val values = Seq(
      Value(0.U, 0.U, 255.U, "b0000_0001".U, "b1001_0111".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0000_0010".U),
      Value(0.U, 1.U, 255.U, "b0000_0000".U, "b1001_0111".U),
      Value(128.U, 128.U, 0.U, "b0000_0000".U, "b0100_0110".U),
      Value(255.U, 1.U, 254.U, "b0000_0000".U, "b1000_0010".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.SBC.U, value, _) }
    }
  }

  "CP" should "compare the operands" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0001".U, "b0100_0010".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0000_0010".U),
      Value(0.U, 1.U, 255.U, "b0000_0000".U, "b1001_0111".U),
      Value(128.U, 128.U, 0.U, "b0000_0000".U, "b0100_0110".U),
      Value(255.U, 1.U, 254.U, "b0000_0000".U, "b1000_0010".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.CP.U, value, _) }
    }
  }

  "AND" should "logical AND the operands" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0101_0100".U),
      Value(1.U, 0.U, 0.U, "b0000_0000".U, "b0101_0100".U),
      Value(1.U, 1.U, 1.U, "b0000_0000".U, "b0001_0000".U),
      Value(255.U, 0.U, 0.U, "b0000_0000".U, "b0101_0100".U),
      Value(0.U, 255.U, 0.U, "b0000_0000".U, "b0101_0100".U),
      Value(255.U, 255.U, 255.U, "b0000_0000".U, "b1001_0100".U),
    )

    for (value <- values) {
        test(new ALU) { testALU(Ops.AND.U, value, _) }
    }
  }

  "OR" should "logical OR the operands" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(0.U, 1.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(1.U, 1.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(255.U, 0.U, 255.U, "b0000_0000".U, "b1000_0100".U),
      Value(0.U, 255.U, 255.U, "b0000_0000".U, "b1000_0100".U),
      Value(255.U, 255.U, 255.U, "b0000_0000".U, "b1000_0100".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.OR.U, value, _) }
    }
  }

  "XOR" should "logical XOR the operands" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(0.U, 1.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(1.U, 1.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(255.U, 0.U, 255.U, "b0000_0000".U, "b1000_0100".U),
      Value(0.U, 255.U, 255.U, "b0000_0000".U, "b1000_0100".U),
      Value(255.U, 255.U, 0.U, "b0000_0000".U, "b0100_0100".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.XOR.U, value, _) }
    }
  }

  "INC" should "increment the operand" in {
    val values = Seq(
      Value(0.U, 0.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(0.U, 0.U, 1.U, "b0000_0001".U, "b0000_0001".U),
      Value(1.U, 0.U, 2.U, "b0000_0000".U, "b0000_0000".U),
      Value(127.U, 0.U, 128.U, "b0000_0000".U, "b1001_0100".U),
      Value(255.U, 1.U, 0.U, "b0000_0000".U, "b0101_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.INC.U, value, _) }
    }
  }

  "DEC" should "decrement the operand" in {
    val values = Seq(
      Value(0.U, 0.U, 255.U, "b0000_0000".U, "b1001_0010".U),
      Value(0.U, 0.U, 255.U, "b0000_0001".U, "b1001_0011".U),
      Value(1.U, 0.U, 0.U, "b0000_0000".U, "b0100_0010".U),
      Value(128.U, 0.U, 127.U, "b0000_0000".U, "b0001_0110".U),
      Value(255.U, 1.U, 254.U, "b0000_0000".U, "b1000_0010".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.DEC.U, value, _) }
    }
  }

  "BIT" should "test whether a bit is set" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0101_0100".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0001_0000".U),
      Value(0.U, 1.U, 0.U, "b0000_0000".U, "b0101_0100".U),
      Value(128.U, 7.U, 128.U, "b0000_0000".U, "b1001_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.BIT.U, value, _) }
    }
  }

  "SET" should "set a bit" in {
    val values = Seq(
      Value(0.U, 0.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(1.U, 0.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(0.U, 1.U, 2.U, "b0000_0000".U, "b0000_0000".U),
      Value(128.U, 7.U, 128.U, "b0000_0000".U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.SET.U, value, _) }
    }
  }

  "RES" should "reset a bit" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0000_0000".U),
      Value(1.U, 0.U, 0.U, "b0000_0000".U, "b0000_0000".U),
      Value(0.U, 1.U, 0.U, "b0000_0000".U, "b0000_0000".U),
      Value(255.U, 7.U, 127.U, "b0000_0000".U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.RES.U, value, _) }
    }
  }

  "RL" should "rotate the bits to the left" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 3.U, "b0000_0001".U, "b0000_0100".U),
      Value(64.U, 0.U, 128.U, "b0000_0000".U, "b1000_0000".U),
      Value(128.U, 0.U, 0.U, "b0000_0000".U, "b0100_0101".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.RL.U, value, _) }
    }
  }

  "RLC" should "rotate the bits to the left (circular)" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 2.U, "b0000_0001".U, "b0000_0000".U),
      Value(64.U, 0.U, 128.U, "b0000_0000".U, "b1000_0000".U),
      Value(128.U, 0.U, 1.U, "b0000_0000".U, "b0000_0001".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.RLC.U, value, _) }
    }
  }

  "RR" should "rotate the bits to the right" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 128.U, "b0000_0001".U, "b1000_0001".U),
      Value(64.U, 0.U, 32.U, "b0000_0000".U, "b0000_0000".U),
      Value(128.U, 0.U, 64.U, "b0000_0000".U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.RR.U, value, _) }
    }
  }

  "RRC" should "rotate the bits to the right (circular)" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 128.U, "b0000_0000".U, "b1000_0001".U),
      Value(64.U, 0.U, 32.U, "b0000_0000".U, "b0000_0000".U),
      Value(128.U, 0.U, 64.U, "b0000_0000".U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.RRC.U, value, _) }
    }
  }

  "SLA" should "shift the bits to the left (arithmetic)" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 2.U, "b0000_0000".U, "b0000_0000".U),
      Value(64.U, 0.U, 128.U, "b0000_0000".U, "b1000_0000".U),
      Value(128.U, 0.U, 0.U, "b0000_0000".U, "b0100_0101".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.SLA.U, value, _) }
    }
  }

  "SLL" should "shift the bits to the left (logical)" in {
    val values = Seq(
      Value(0.U, 0.U, 1.U, "b0000_0000".U, "b0000_0000".U),
      Value(1.U, 0.U, 3.U, "b0000_0000".U, "b0000_0100".U),
      Value(64.U, 0.U, 129.U, "b0000_0000".U, "b1000_0100".U),
      Value(128.U, 0.U, 1.U, "b0000_0000".U, "b0000_0001".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.SLL.U, value, _) }
    }
  }

  "SRA" should "shift the bits to the right (arithmetic)" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 0.U, "b0000_0000".U, "b0100_0101".U),
      Value(64.U, 0.U, 32.U, "b0000_0000".U, "b0000_0000".U),
      Value(128.U, 0.U, 192.U, "b0000_0000".U, "b1000_0100".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.SRA.U, value, _) }
    }
  }

  "SRL" should "shift the bits to the right (logical)" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(1.U, 0.U, 0.U, "b0000_0000".U, "b0100_0101".U),
      Value(64.U, 0.U, 32.U, "b0000_0000".U, "b0000_0000".U),
      Value(128.U, 0.U, 64.U, "b0000_0000".U, "b0000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.SRL.U, value, _) }
    }
  }

  "RLD" should "rotate the upper BCD digit" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(48.U, 16.U, 49.U, "b0000_0000".U, "b0000_0000".U),
      Value(144.U, 16.U, 145.U, "b0000_0000".U, "b1000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.RLD.U, value, _) }
    }
  }

  "RRD" should "rotate the lower BCD digit" in {
    val values = Seq(
      Value(0.U, 0.U, 0.U, "b0000_0000".U, "b0100_0100".U),
      Value(48.U, 1.U, 49.U, "b0000_0000".U, "b0000_0000".U),
      Value(144.U, 1.U, 145.U, "b0000_0000".U, "b1000_0000".U),
    )

    for (value <- values) {
      test(new ALU) { testALU(Ops.RRD.U, value, _) }
    }
  }
}
