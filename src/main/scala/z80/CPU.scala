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
import chisel3.util.{is, _}

/** 8-bit registers */
object Reg8 {
  val A = 0; val F = 1
  val B = 2; val C = 3
  val D = 4; val E = 5
  val H = 6; val L = 7
}

/** 16-bit registers */
object Reg16 {
  val AF = 0
  val BC = 1
  val DE = 2
  val HL = 3
  val IX = 4
  val IY = 5
  val SP = 6
  val PC = 7
}

/** Z80 CPU */
class CPU extends Module {
  val io = IO(new Bundle {
    /** Memory request */
    val mreq = Output(Bool())
    /** Read */
    val rd = Output(Bool())
    /** Write */
    val wr = Output(Bool())
    /** Halt state */
    val halt = Output(Bool())
    /** Address bus */
    val addr = Output(UInt(CPU.ADDR_WIDTH.W))
    /** Data input */
    val din = Input(UInt(CPU.DATA_WIDTH.W))
    /** Data output */
    val dout = Output(UInt(CPU.DATA_WIDTH.W))
    /** M1 cycle */
    val m1 = Output(Bool())
    /** Debug output */
    val debug = new Bundle {
      val tState = Output(UInt(2.W))
      val mCycle = Output(UInt(2.W))
      val registers8 = Output(Vec(CPU.NUM_REG_8, UInt(8.W)))
      val registers16 = Output(Vec(CPU.NUM_REG_16, UInt(16.W)))
    }
  })

  // Time state counter
  val tStates = Wire(UInt(3.W))
  val (tStateCounter, tStateWrap) = Counter(tStates)

  // Machine cycle counter
  val mCycles = Wire(UInt(3.W))
  val (mCycleCounter, mCycleWrap) = Counter(mCycles, enable = tStateWrap)

  // Program counter register
  val pcReg = RegInit(0.U(CPU.ADDR_WIDTH.W))

  // Instruction register
  val instructionReg = RegInit(0.U(CPU.DATA_WIDTH.W))

  // Temporary register
  val tempReg = RegInit(0.U(CPU.DATA_WIDTH.W))

  // Halt register
  val haltReg = RegInit(false.B)

  // Register files
  val registers16 = RegInit(VecInit(Seq.fill(CPU.NUM_REG_16) { 0.U(16.W) }))
  val registers8 = RegInit(VecInit(registers16.take(CPU.NUM_REG_8/2).flatMap { r => Seq(r(15, 8), r(7, 0)) }))

  // Instruction decoder
  val decoder = Module(new Decoder)
  decoder.io.instruction := instructionReg
  decoder.io.tState := tStateCounter
  decoder.io.mCycle := mCycleCounter
  tStates := decoder.io.tStates
  mCycles := decoder.io.mCycles

  // Arithmetic logic unit
  val alu = Module(new ALU)
  alu.io.op := decoder.io.op
  alu.io.a := tempReg
  alu.io.b := registers8(Reg8.A.U)
  alu.io.flagsIn := registers8(Reg8.F)

  // Default outputs
  io.mreq := false.B
  io.rd := false.B
  io.wr := false.B
  io.halt := haltReg
  io.addr := 0.U
  io.dout := 0.U
  io.m1 := false.B
  io.debug.tState := tStateCounter
  io.debug.mCycle := mCycleCounter
  io.debug.registers8 := registers8
  io.debug.registers16 := registers16

  printf(p"T: $tStateCounter ($tStates), M: $mCycleCounter ($mCycles), PC: $pcReg, IR: $instructionReg, tempReg: $tempReg, aluResult: ${alu.io.result}, srcIndex: ${decoder.io.srcIndex}, destIndex: ${decoder.io.destIndex}\n")

  switch(tStateCounter) {
    is(0.U) {
      // Increment program counter
      pcReg := pcReg + 1.U

      // Place program counter on the address bus
      io.addr := pcReg

      // Assert M1 during first machine cycle
      io.m1 := mCycleCounter === 0.U
    }

    is(1.U) {
      // Fetch instruction
      io.mreq := true.B
      io.rd := true.B

      // Assert M1 during first machine cycle
      io.m1 := mCycleCounter === 0.U

      // Latch an instruction only during first machine cycle. If the CPU is halted
      // then a NOP is forced.
      when(mCycleCounter === 0.U) {
        instructionReg := Mux(haltReg, Instructions.NOP.U, io.din)
      }

      // Read the data input to the temp register
      when(decoder.io.rd) {
        tempReg := io.din
      } otherwise {
        tempReg := registers8(decoder.io.srcIndex)
      }
    }

    is(2.U) {
      when(decoder.io.rd) {
        registers8(decoder.io.destIndex) := tempReg
      }
    }

    is(3.U) {
      // Write the result from the ALU to the register bus
      when(decoder.io.store) {
        registers8(decoder.io.destIndex) := alu.io.result
      }

      // Write the flags from the ALU to the F register
      registers8(Reg8.F) := alu.io.flagsOut

      // Set halt register
      haltReg := haltReg || decoder.io.halt
    }
  }
}

object CPU {
  val ADDR_WIDTH = 16
  val DATA_WIDTH = 8
  val NUM_REG_8 = 8
  val NUM_REG_16 = 8
}
