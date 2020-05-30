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

/**
 * 16-bit registers
 */
object Reg16 {
  val AF = 0.U
  val BC = 1.U
  val DE = 2.U
  val HL = 3.U
  val SP = 4.U
  val PC = 5.U
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
  val DATA_WIDTH = 8.W
  val ADDR_WIDTH = 16.W

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
    val addr = Output(UInt(ADDR_WIDTH))

    /**
     * data input
     */
    val din = Input(UInt(DATA_WIDTH))

    /**
     * data output
     */
    val dout = Output(UInt(DATA_WIDTH))

    /**
     * M1 cycle
     */
    val m1 = Output(Bool())

    /**
     * program counter
     */
    val pc = Output(UInt(ADDR_WIDTH))
  })

  // 16-bit register file
  val registers16 = RegInit(VecInit(Seq.fill(8) { 0.U(16.W) }))

  // 8-bit register file
  val registers8 = Reg(Vec(16, UInt(8.W)))
  registers8 := registers16.flatMap { r => Seq(r(15, 8), r(7, 0)) }

  // register aliases
  val pc = registers16(Reg16.PC)
  val f = registers8(Reg8.F)

  // instruction register
  val ir = RegInit(0.U(DATA_WIDTH))

  // data input register
  val dataIn = RegNext(io.din, 0.U(DATA_WIDTH))

  // internal data buses
  val a = Wire(UInt(DATA_WIDTH))
  val b = Wire(UInt(DATA_WIDTH))

  // instruction decoder
  val decoder = Module(new Decoder)
  decoder.io.ir := ir

  // arithmetic logic unit
  val alu = Module(new ALU)
  alu.io.op := decoder.io.op
  alu.io.a := a
  alu.io.b := b
  alu.io.flagsIn := f

  // FIXME: set internal buses with decoder output
  a := registers8(decoder.io.indexA)
  b := registers8(decoder.io.indexB)

  // increment program counter
  pc := pc + 1.U

  // default outputs
  io.mreq := false.B
  io.rd := false.B
  io.wr := false.B
  io.halt := false.B
  io.addr := 0.U
  io.dout := 0.U
  io.m1 := false.B
  io.pc := pc
}
