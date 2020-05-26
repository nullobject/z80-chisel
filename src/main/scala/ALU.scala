import chisel3._
import chisel3.util._

/**
 * Operations
 */
object Ops {
  val nop :: add :: sub :: Nil = Enum(3)
}

class ALU extends Module {
  val io = IO(new Bundle {
    /**
     * The operation to execute
     */
    val op = Input(UInt(4.W))

    /**
     * Operand A
     */
    val a = Input(UInt(8.W))

    /**
     * Operand B
     */
    val b = Input(UInt(8.W))

    /**
     * The result of the operation
     */
    val q = Output(UInt(8.W))

    /**
     * The metadata for the calculation
     */
    val flags = Output(UInt(8.W))
  })

  // default output
  io.q := 0.U

  switch (io.op) {
    is (Ops.add) {
      io.q := io.a + io.b
    }
    is (Ops.sub) {
      io.q := io.a - io.b
    }
  }

  io.flags := 0.U
}
