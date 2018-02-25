package protoflex

import chisel3._
import chisel3.util.{BitPat, ListLookup, MuxLookup}

class DInst extends Bundle
{
  // Data
  val rd  = REG_T
  val rs1 = REG_T
  val rs2 = REG_T
  val imm = IMM_T
  val shift = SHIFT_T

  // Control
  val imm_valid = Bool()
  val aluOp = OP_ALU_T
  val shift_v = Bool()
  val valid = Bool()

  def decode(inst : UInt) = {
    val decoder = ListLookup(inst, Decode.decode_control_default, Decode.table_control)

    // Data
    val dtype = decoder.head
    rd    := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst(4,0) ))
    rs1   := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst(4,0) ))
    rs2   := MuxLookup(dtype,   REG_X, Array( I_LogSR -> inst(4,0) ))
    imm   := MuxLookup(dtype,   IMM_X, Array( I_LogSR -> Cat(0.U(20.W), inst(15,10))))
    shift := MuxLookup(dtype, SHIFT_X, Array( I_LogSR -> inst(23,22) ))

    // Control
    val cdecoder = decoder.tail
    val csignals = Seq(imm_valid, aluOp, shift_v, valid)
    csignals zip cdecoder map { case (s, d) => s:= d }

    this
  }
}

object Decode
{
  def decode_control_default: List[UInt] =
    //
    //              ALU   INSTRUCTION
    // INSTR         OP     VALID
    // TYPE          |        |
    //   |   IMM     |   SHIFT|
    //   |  VALID    |   VALID|
    //   |    |      |     |  |
    //   |    |      |     |  |
    List(I_X, N, OP_ALU_X, N, N)

  def table_control: Array[(BitPat, List[UInt])]  =
    Array(
      /* Logical (shifted register) 64-bit */
      AND  -> List(I_LogSR, N, OP_AND, Y, Y),
      BIC  -> List(I_LogSR, N, OP_BIC, Y, Y),
      ORR  -> List(I_LogSR, N, OP_ORR, Y, Y),
      ORN  -> List(I_LogSR, N, OP_ORN, Y, Y),
      EOR  -> List(I_LogSR, N, OP_EOR, Y, Y),
      EON  -> List(I_LogSR, N, OP_EON, Y, Y)
    )
}

class DecodeUnitIo extends Bundle
{
  val inst = Input(UInt(32.W))
  val dinst = Output(new DInst)
}

class DecodeUnit extends Module
{
  val io = new DecodeUnitIo
  io.dinst := Wire(new DInst).decode(io.inst)
}
