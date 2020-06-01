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
import chisel3.util._

/**
 * 16-bit registers
 */
object Reg16 {
  val AF = 0.U
  val BC = 1.U
  val DE = 2.U
  val HL = 3.U
  val IX = 4.U
  val IY = 5.U
  val SP = 6.U
  val WZ = 7.U // internal
}

/**
 * 8-bit registers
 */
object Reg8 {
  val A = 0.U; val F = 1.U
  val B = 2.U; val C = 3.U
  val D = 4.U; val E = 5.U
  val H = 6.U; val L = 7.U
}

/**
 * Z80 CPU
 */
class CPU extends Module {
  val DATA_WIDTH = 8
  val ADDR_WIDTH = 16

  val io = IO(new Bundle {
    /**
     * memory request
     */
    val mreq = Output(Bool())

    /**
     * read
     */
    val rd = Output(Bool())

    /**
     * write
     */
    val wr = Output(Bool())

    /**
     * halt state
     */
    val halt = Output(Bool())

    /**
     * address bus
     */
    val addr = Output(UInt(ADDR_WIDTH.W))

    /**
     * data input
     */
    val din = Input(UInt(DATA_WIDTH.W))

    /**
     * data output
     */
    val dout = Output(UInt(DATA_WIDTH.W))

    /**
     * M1 cycle
     */
    val m1 = Output(Bool())

    /**
     * debug output
     */
    val registers8  = Output(Vec(16, UInt(8.W)))
    val registers16 = Output(Vec(8, UInt(16.W)))
  })

  // 16-bit register file
  val registers16 = RegInit(VecInit(Seq.fill(8) { 0.U(16.W) }))

  // 8-bit register file
  val registers8 = Reg(Vec(16, UInt(8.W)))
  registers8 := registers16.flatMap { r => Seq(r(15, 8), r(7, 0)) }

  // instruction register
  val pc = RegInit(0.U(ADDR_WIDTH.W))
  val ir = RegInit(0.U(DATA_WIDTH.W))

  // data input register
  val dataIn = RegNext(io.din, 0.U(DATA_WIDTH.W))

  // instruction decoder
  val decoder = Module(new Decoder)
  decoder.io.ir := ir

  // arithmetic logic unit
  val alu = Module(new ALU)
  alu.io.op := decoder.io.op
  alu.io.a := registers8(Reg8.A)
  alu.io.b := registers8(decoder.io.indexB)
  alu.io.flagsIn := registers8(Reg8.F)

  val halt = RegInit(false.B)

  // default outputs
  io.mreq := false.B
  io.rd := false.B
  io.wr := false.B
  io.halt := halt
  io.addr := 0.U
  io.dout := 0.U
  io.m1 := false.B

  val t1 :: t2 :: t3 :: t4 :: Nil = Enum(4)
  val stateReg = RegInit(t1)

  switch (stateReg) {
    is (t1) {
      // place the program counter on the address bus
      io.addr := pc

      stateReg := t2
    }
    is (t2) {
      // fetch instruction
      io.mreq := true.B
      io.rd := true.B

      stateReg := t3
    }
    is (t3) {
      // latch instruction
      ir := io.din

      stateReg := t4
    }
    is (t4) {
      // store ALU result to the target register
      registers8(decoder.io.indexA) := alu.io.result

      // increment program counter
      when (!halt) { pc := pc + 1.U }

      stateReg := t1
    }
  }

  // set debug output
  io.registers8 <> registers8
  io.registers16 <> registers16
}
