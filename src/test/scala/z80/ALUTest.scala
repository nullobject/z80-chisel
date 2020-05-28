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
  behavior of "ADD"

  it should "add the inputs (without carry)" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0011".U)
      c.io.result.expect(3.U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(128.U)
      c.io.b.poke(0.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(0.U)
      c.io.b.poke(0.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0000".U)
    }
  }

  it should "set the half-carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(15.U)
      c.io.b.poke(1.U)
      c.io.result.expect(16.U)
      c.io.flagsOut.expect("b0001_0000".U)
    }
  }

  it should "set the overflow flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(64.U)
      c.io.b.poke(64.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.add)
      c.io.a.poke(255.U)
      c.io.b.poke(1.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0101_0001".U)
    }
  }

  behavior of "ADC"

  it should "add the inputs (with carry)" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.adc)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0011".U)
      c.io.result.expect(4.U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.adc)
      c.io.a.poke(128.U)
      c.io.b.poke(0.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.adc)
      c.io.a.poke(0.U)
      c.io.b.poke(0.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0000".U)
    }
  }

  it should "set the half-carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.adc)
      c.io.a.poke(15.U)
      c.io.b.poke(1.U)
      c.io.result.expect(16.U)
      c.io.flagsOut.expect("b0001_0000".U)
    }
  }

  it should "set the overflow flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.adc)
      c.io.a.poke(64.U)
      c.io.b.poke(64.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.adc)
      c.io.a.poke(255.U)
      c.io.b.poke(1.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0101_0001".U)
    }
  }

  behavior of "SUB"

  it should "subtract the inputs (without carry)" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect(1.U)
      c.io.flagsOut.expect("b0000_0010".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(0.U)
      c.io.b.poke(128.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0011".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(1.U)
      c.io.b.poke(1.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0010".U)
    }
  }

  it should "set the half-carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(16.U)
      c.io.b.poke(1.U)
      c.io.result.expect(15.U)
      c.io.flagsOut.expect("b0001_0010".U)
    }
  }

  it should "set the overflow flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(0.U)
      c.io.b.poke(1.U)
      c.io.result.expect(255.U)
      c.io.flagsOut.expect("b1001_0111".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sub)
      c.io.a.poke(0.U)
      c.io.b.poke(192.U)
      c.io.result.expect(64.U)
      c.io.flagsOut.expect("b0000_0011".U)
    }
  }

  behavior of "SBC"

  it should "subtract the inputs (with carry)" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sbc)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0010".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sbc)
      c.io.a.poke(0.U)
      c.io.b.poke(128.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0011".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sbc)
      c.io.a.poke(1.U)
      c.io.b.poke(1.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0010".U)
    }
  }

  it should "set the half-carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sbc)
      c.io.a.poke(16.U)
      c.io.b.poke(1.U)
      c.io.result.expect(15.U)
      c.io.flagsOut.expect("b0001_0010".U)
    }
  }

  it should "set the overflow flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sbc)
      c.io.a.poke(0.U)
      c.io.b.poke(1.U)
      c.io.result.expect(255.U)
      c.io.flagsOut.expect("b1001_0111".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sbc)
      c.io.a.poke(0.U)
      c.io.b.poke(192.U)
      c.io.result.expect(64.U)
      c.io.flagsOut.expect("b0000_0011".U)
    }
  }

  behavior of "CP"

  it should "compare the inputs" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.cp)
      c.io.a.poke(2.U)
      c.io.b.poke(1.U)
      c.io.result.expect(1.U)
      c.io.flagsOut.expect("b0000_0010".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.cp)
      c.io.a.poke(0.U)
      c.io.b.poke(128.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0011".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.cp)
      c.io.a.poke(1.U)
      c.io.b.poke(1.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0010".U)
    }
  }

  it should "set the half-carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.cp)
      c.io.a.poke(16.U)
      c.io.b.poke(1.U)
      c.io.result.expect(15.U)
      c.io.flagsOut.expect("b0001_0010".U)
    }
  }

  it should "set the overflow flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.cp)
      c.io.a.poke(0.U)
      c.io.b.poke(1.U)
      c.io.result.expect(255.U)
      c.io.flagsOut.expect("b1001_0111".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.cp)
      c.io.a.poke(0.U)
      c.io.b.poke(192.U)
      c.io.result.expect(64.U)
      c.io.flagsOut.expect("b0000_0011".U)
    }
  }

  behavior of "AND"

  it should "logical AND the inputs" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.and)
      c.io.a.poke(1.U)
      c.io.b.poke(1.U)
      c.io.flagsIn.poke("b0000_0011".U)
      c.io.result.expect(1.U)
      c.io.flagsOut.expect("b0001_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.and)
      c.io.a.poke(128.U)
      c.io.b.poke(128.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1001_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.and)
      c.io.a.poke(1.U)
      c.io.b.poke(0.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0101_0100".U)
    }
  }

  behavior of "XOR"

  it should "logical XOR the inputs" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.xor)
      c.io.a.poke(1.U)
      c.io.b.poke(0.U)
      c.io.flagsIn.poke("b0000_0011".U)
      c.io.result.expect(1.U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.xor)
      c.io.a.poke(128.U)
      c.io.b.poke(0.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.xor)
      c.io.a.poke(1.U)
      c.io.b.poke(1.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  behavior of "OR"

  it should "logical OR the inputs" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.or)
      c.io.a.poke(1.U)
      c.io.b.poke(0.U)
      c.io.flagsIn.poke("b0000_0011".U)
      c.io.result.expect(1.U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.or)
      c.io.a.poke(128.U)
      c.io.b.poke(0.U)
      c.io.result.expect(128.U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.or)
      c.io.a.poke(0.U)
      c.io.b.poke(0.U)
      c.io.result.expect(0.U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  behavior of "RL"

  it should "rotate A left though carry" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rl)
      c.io.a.poke("b0000_0001".U)
      c.io.flagsIn.poke("b0001_0011".U)
      c.io.result.expect("b0000_0011".U)
      c.io.flagsOut.expect("b0000_0100".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rl)
      c.io.a.poke("b0100_0000".U)
      c.io.result.expect("b1000_0000".U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rl)
      c.io.a.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rl)
      c.io.a.poke("b1000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0101".U)
    }
  }

  behavior of "RLC"

  it should "rotate A left circular" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rlc)
      c.io.a.poke("b0000_0001".U)
      c.io.result.expect("b0000_0010".U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rlc)
      c.io.a.poke("b0100_0000".U)
      c.io.result.expect("b1000_0000".U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rlc)
      c.io.a.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rlc)
      c.io.a.poke("b1000_0000".U)
      c.io.result.expect("b0000_0001".U)
      c.io.flagsOut.expect("b0000_0001".U)
    }
  }

  behavior of "RR"

  it should "rotate A right through carry" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rr)
      c.io.a.poke("b0000_0001".U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect("b1000_0000".U)
      c.io.flagsOut.expect("b1000_0001".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rr)
      c.io.a.poke("b0000_0000".U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect("b1000_0000".U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rr)
      c.io.a.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rr)
      c.io.a.poke("b0000_0001".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0101".U)
    }
  }

  behavior of "RRC"

  it should "rotate A right circular" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rrc)
      c.io.a.poke("b0000_0010".U)
      c.io.result.expect("b0000_0001".U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rrc)
      c.io.a.poke("b0000_0001".U)
      c.io.result.expect("b1000_0000".U)
      c.io.flagsOut.expect("b1000_0001".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rrc)
      c.io.a.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rrc)
      c.io.a.poke("b0000_0001".U)
      c.io.result.expect("b1000_0000".U)
      c.io.flagsOut.expect("b1000_0001".U)
    }
  }

  behavior of "SLA"

  it should "shift A left arithmetic" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sla)
      c.io.a.poke("b0000_0001".U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect("b0000_0010".U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sla)
      c.io.a.poke("b0100_0000".U)
      c.io.result.expect("b1000_0000".U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sla)
      c.io.a.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sla)
      c.io.a.poke("b1000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0101".U)
    }
  }

  behavior of "SLL"

  it should "shift A left logical" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sll)
      c.io.a.poke("b0000_0000".U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect("b0000_0001".U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sll)
      c.io.a.poke("b0100_0000".U)
      c.io.result.expect("b1000_0001".U)
      c.io.flagsOut.expect("b1000_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sll)
      c.io.a.poke("b1000_0000".U)
      c.io.result.expect("b0000_0001".U)
      c.io.flagsOut.expect("b0000_0001".U)
    }
  }

  behavior of "SRA"

  it should "shift A right arithmetic" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sra)
      c.io.a.poke("b0000_0010".U)
      c.io.result.expect("b0000_0001".U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sra)
      c.io.a.poke("b1000_0000".U)
      c.io.result.expect("b1100_0000".U)
      c.io.flagsOut.expect("b1000_0100".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sra)
      c.io.a.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.sra)
      c.io.a.poke("b0000_0001".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0101".U)
    }
  }

  behavior of "SRL"

  it should "shift A right logical" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.srl)
      c.io.a.poke("b0000_0010".U)
      c.io.flagsIn.poke("b0000_0001".U)
      c.io.result.expect("b0000_0001".U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.srl)
      c.io.a.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  it should "set the carry flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.srl)
      c.io.a.poke("b0000_0001".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0101".U)
    }
  }

  behavior of "RLD"

  it should "rotate the upper BCD digit between A and B" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rld)
      c.io.a.poke("b0011_0000".U)
      c.io.b.poke("b0001_0000".U)
      c.io.result.expect("b0011_0001".U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rld)
      c.io.a.poke("b1001_0000".U)
      c.io.b.poke("b0001_0000".U)
      c.io.result.expect("b1001_0001".U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rld)
      c.io.a.poke("b0000_0000".U)
      c.io.b.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }

  behavior of "RRD"

  it should "rotate the lower BCD digit between A and B" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rrd)
      c.io.a.poke("b0011_0000".U)
      c.io.b.poke("b0000_0001".U)
      c.io.result.expect("b0011_0001".U)
      c.io.flagsOut.expect("b0000_0000".U)
    }
  }

  it should "set the sign flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rrd)
      c.io.a.poke("b1001_0000".U)
      c.io.b.poke("b0000_0001".U)
      c.io.result.expect("b1001_0001".U)
      c.io.flagsOut.expect("b1000_0000".U)
    }
  }

  it should "set the zero flag" in {
    test(new ALU) { c =>
      c.io.op.poke(Ops.rrd)
      c.io.a.poke("b0000_0000".U)
      c.io.b.poke("b0000_0000".U)
      c.io.result.expect("b0000_0000".U)
      c.io.flagsOut.expect("b0100_0100".U)
    }
  }
}
