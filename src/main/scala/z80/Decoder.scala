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

sealed trait Cycle

/** Represents a cycle where an opcode is fetched */
case class OpcodeFetch(op: Int = Ops.NOP, busIndex: Option[Int] = None, halt: Boolean = false) extends Cycle

/** Represents a cycle where a value is read from memory */
case class MemRead(busIndex: Option[Int] = None, wr: Boolean = false) extends Cycle

/** Represents a cycle where a value is written to memory */
case class MemWrite(busIndex: Option[Int] = None, wr: Boolean = false) extends Cycle

/**
 * Decodes the instruction register value into an operation and address bus
 * indexes.
 */
class Decoder extends Module {
  val io = IO(new Bundle {
    /** Instruction */
    val instruction = Input(UInt(8.W))
    /** Machine cycle */
    val mCycle = Input(UInt(log2Up(CPU.MAX_M_CYCLES).W))
    /** Asserted when the machine cycle counter should be reset */
    val mCycleReset = Output(Bool())
    /** Operation */
    val op = Output(UInt(5.W))
    /** Register bus index */
    val busIndex = Output(UInt(4.W))
    /** Asserted when the result should be stored to the accumulator */
    val wr = Output(Bool())
    /** Halt */
    val halt = Output(Bool())
  })

  /** Decodes the given microcode and sets the operation and register index outputs. */
  private def decodeCycle(cycle: Cycle) = {
    cycle match {
      case OpcodeFetch(op, busIndex, halt) =>
        io.op := op.U
        busIndex match {
          case Some(i) => io.busIndex := i.U
          case None =>
        }
        io.halt := halt.B

      case MemRead(busIndex, store) =>
        io.op := Ops.NOP.U
        busIndex match {
          case Some(i) => io.busIndex := i.U
          case None =>
        }
        io.wr := store.B
    }
  }

  // Default outputs
  io.op := 0.U
  io.busIndex := 0.U
  io.halt := false.B
  io.mCycleReset := false.B
  io.wr := false.B

  // Decode the instructions
  for ((code, cycles) <- Decoder.instructions) {
    when(io.instruction === code.U) {
      for ((cycle, i) <- cycles.zipWithIndex) {
        when(io.mCycle === i.U) {
          decodeCycle(cycle)

          // Reset the machine cycle during the last cycle of the instruction
          io.mCycleReset := (i == cycles.length-1).B
        }
      }
    }
  }
}

object Decoder {
  import Instructions._

  val instructions = Seq(
    (NOP   -> Seq(OpcodeFetch())),
    (INC_A -> Seq(OpcodeFetch(op = Ops.INC, busIndex = Some(Reg8.A)))),
    (INC_B -> Seq(OpcodeFetch(op = Ops.INC, busIndex = Some(Reg8.B)))),
    (LD_A  -> Seq(OpcodeFetch(), MemRead(busIndex = Some(Reg8.A), wr = true))),
    (HALT  -> Seq(OpcodeFetch(halt = true))),
  )
}
