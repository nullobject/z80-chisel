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

sealed trait Cycle

/** Represents a cycle where an opcode is fetched */
case class OpcodeFetch(op: Int = Ops.NOP, srcIndex: Option[Int] = None, destIndex: Option[Int] = None, store: Boolean = false, halt: Boolean = false) extends Cycle

/** Represents a cycle where a value is read from memory */
case class MemRead(srcIndex: Option[Int] = None, destIndex: Option[Int] = None, store: Boolean = false) extends Cycle

/** Represents a cycle where a value is written to memory */
case class MemWrite(srcIndex: Option[Int] = None, destIndex: Option[Int] = None, store: Boolean = false) extends Cycle

/** Decodes the current CPU state value into a number of control signals. */
class Decoder extends Module {
  val io = IO(new Bundle {
    /** Instruction */
    val instruction = Input(UInt(8.W))
    /** Time state */
    val tState = Input(UInt(3.W))
    /** Machine cycle */
    val mCycle = Input(UInt(3.W))
    /** Number of time states required for the current machine cycle */
    val tStates = Output(UInt(3.W))
    /** Number of machine cycles required for the current instruction */
    val mCycles = Output(UInt(3.W))
    /** Operation */
    val op = Output(UInt(5.W))
    /** Register bus source index */
    val srcIndex = Output(UInt(4.W))
    /** Register bus destination index */
    val destIndex = Output(UInt(4.W))
    /** Asserted when the result should be stored to the accumulator */
    val store = Output(Bool())
    /** Asserted during a read cycle */
    val rd = Output(Bool())
    /** Asserted during a write cycle */
    val wr = Output(Bool())
    /** Halt */
    val halt = Output(Bool())
  })

  /** Decodes the given microcode and sets the operation and register index outputs. */
  private def decodeCycle(cycle: Cycle) {
    cycle match {
      case cycle: OpcodeFetch =>
        io.op := cycle.op.U
        cycle.srcIndex.foreach(io.srcIndex := _.U)
        cycle.destIndex.foreach(io.destIndex := _.U)
        io.store := cycle.store.B
        io.halt := cycle.halt.B

      case cycle: MemRead =>
        io.tStates := 3.U
        cycle.srcIndex.foreach(io.srcIndex := _.U)
        cycle.destIndex.foreach(io.destIndex := _.U)
        io.store := cycle.store.B
        io.rd := true.B

      case cycle: MemWrite =>
        io.tStates := 3.U
        cycle.srcIndex.foreach(io.srcIndex := _.U)
        cycle.destIndex.foreach(io.destIndex := _.U)
        io.store := cycle.store.B
        io.wr := true.B
    }
  }

  // Default outputs
  io.tStates := 4.U
  io.mCycles := 1.U
  io.op := Ops.NOP.U
  io.srcIndex := Reg8.A.U
  io.destIndex := Reg8.A.U
  io.store := false.B
  io.rd := false.B
  io.wr := false.B
  io.halt := false.B

  // Decode the instructions
  for ((code, cycles) <- Decoder.instructions) {
    when(io.instruction === code.U) {
      io.mCycles := cycles.length.U
      for ((cycle, i) <- cycles.zipWithIndex) {
        when(io.mCycle === i.U) {
          decodeCycle(cycle)
        }
      }
    }
  }
}

object Decoder {
  import Instructions._

  val instructions = Seq(
    NOP   -> Seq(OpcodeFetch()),
    INC_A -> Seq(OpcodeFetch(op = Ops.INC, destIndex = Some(Reg8.A), store = true)),
    INC_B -> Seq(OpcodeFetch(op = Ops.INC, destIndex = Some(Reg8.B), store = true)),
    LD_A  -> Seq(OpcodeFetch(), MemRead(destIndex = Some(Reg8.A), store = true)),
    LD_B  -> Seq(OpcodeFetch(), MemRead(destIndex = Some(Reg8.B), store = true)),
    ADD_A -> Seq(OpcodeFetch(op = Ops.ADD, srcIndex = Some(Reg8.A), destIndex = Some(Reg8.A), store = true)),
    ADD_B -> Seq(OpcodeFetch(op = Ops.ADD, srcIndex = Some(Reg8.B), destIndex = Some(Reg8.A), store = true)),
    HALT  -> Seq(OpcodeFetch(halt = true))
  )
}
