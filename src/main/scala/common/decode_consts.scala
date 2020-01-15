package common

import chisel3._
import chisel3.util.BitPat
import DECODE_CONTROL_SIGNALS.{N, Y, _}

object DECODE_CONTROL_SIGNALS
{
  // Valault Defues
  val IMM_W = DEC_LITS.IMM_W.W
  def IMM_T = UInt(IMM_W)
  val IMM_X = DEC_LITS.IMM_X.U(IMM_W)

  // Controls Signals
  def C_T = Bool()
  val Y = true.B
  val N = false.B

  // Inst OP's
  val OP_W = DEC_LITS.OP_W.W
  def OP_T = UInt(OP_W)
  val OP_X = DEC_LITS.OP_X.U(OP_W)

  // PC-rel
  val OP_ADR  = DEC_LITS.OP_ADR.U(OP_W)
  val OP_ADRP = DEC_LITS.OP_ADRP.U(OP_W)

  // ALU Operation Signals
  val OP_AND = DEC_LITS.OP_AND.U(OP_W)
  val OP_BIC = DEC_LITS.OP_BIC.U(OP_W)
  val OP_ORR = DEC_LITS.OP_ORR.U(OP_W)
  val OP_ORN = DEC_LITS.OP_ORN.U(OP_W)
  val OP_EOR = DEC_LITS.OP_EOR.U(OP_W)
  val OP_EON = DEC_LITS.OP_EON.U(OP_W)

  // Add/Substract
  val OP_ADD = DEC_LITS.OP_ADD.U(OP_W)
  val OP_SUB = DEC_LITS.OP_SUB.U(OP_W)

  // Branches Signals
  val OP_B = DEC_LITS.OP_B.U(OP_W)
  val OP_BL = DEC_LITS.OP_BL.U(OP_W)
  val OP_BCOND = DEC_LITS.OP_BCOND.U(OP_W)

  val OP_CBZ = DEC_LITS.OP_CBZ.U(OP_W)
  val OP_CBNZ = DEC_LITS.OP_CBZ.U(OP_W)

  // Move wide (immediate)
  val OP_MOVN = DEC_LITS.OP_MOVN.U(OP_W)
  val OP_MOVZ = DEC_LITS.OP_MOVZ.U(OP_W)
  val OP_MOVK = DEC_LITS.OP_MOVK.U(OP_W)

  // Bitfield
  val OP_SBFM = DEC_LITS.OP_SBFM.U(OP_W)
  val OP_BFM  = DEC_LITS.OP_BFM.U(OP_W)
  val OP_UBFM = DEC_LITS.OP_UBFM.U(OP_W)

  // Conditional select
  val OP_CSEL  = DEC_LITS.OP_CSEL.U(OP_W)
  val OP_CSINC = DEC_LITS.OP_CSINC.U(OP_W)
  val OP_CSINV = DEC_LITS.OP_CSINV.U(OP_W)
  val OP_CSNEG = DEC_LITS.OP_CSNEG.U(OP_W)

  // Conditional Compare
  val OP_CCMN = DEC_LITS.OP_CCMN.U(OP_W)
  val OP_CCMP = DEC_LITS.OP_CCMP.U(OP_W)

  // Shiift Operation Signals
  val SHIFT_VAL_W = DEC_LITS.SHIFT_VAL_W.W
  def SHIFT_VAL_T = UInt(SHIFT_VAL_W)
  val SHIFT_VAL_X = DEC_LITS.SHIFT_VAL_X.U(SHIFT_VAL_W)
  val SHIFT_TYPE_W = DEC_LITS.SHIFT_TYPE_W.W
  def SHIFT_TYPE_T = UInt(SHIFT_TYPE_W)
  val SHIFT_TYPE_X = DEC_LITS.SHIFT_TYPE_X.U(SHIFT_TYPE_W)
  val LSL = DEC_LITS.LSL.U(SHIFT_TYPE_W)
  val LSR = DEC_LITS.LSR.U(SHIFT_TYPE_W)
  val ASR = DEC_LITS.ASR.U(SHIFT_TYPE_W)
  val ROR = DEC_LITS.ROR.U(SHIFT_TYPE_W)

  // Cond Operations Signals
  val COND_W = DEC_LITS.COND_W.W
  def COND_T = UInt(COND_W)
  val COND_X = DEC_LITS.COND_X.U(COND_W)
  // Not needed, already in 4 bit form
  // val EQ = DEC_LITS.EQ.U(COND_W)
  // val NE = DEC_LITS.NE.U(COND_W)
  // val CS = DEC_LITS.CS.U(COND_W)
  // val HS = DEC_LITS.HS.U(COND_W)
  // val CC = DEC_LITS.CC.U(COND_W)
  // val LO = DEC_LITS.LO.U(COND_W)
  // val MI = DEC_LITS.MI.U(COND_W)
  // val PL = DEC_LITS.PL.U(COND_W)
  // val VS = DEC_LITS.VS.U(COND_W)
  // val VC = DEC_LITS.VC.U(COND_W)
  // val HI = DEC_LITS.HI.U(COND_W)
  // val LS = DEC_LITS.LS.U(COND_W)
  // val GE = DEC_LITS.GE.U(COND_W)
  // val LT = DEC_LITS.LT.U(COND_W)
  // val GT = DEC_LITS.GT.U(COND_W)
  // val LE = DEC_LITS.LE.U(COND_W)
  // val AL = DEC_LITS.AL.U(COND_W)
  // val NV = DEC_LITS.NV.U(COND_W)

  // load/store signal
  val OP_LDR = DEC_LITS.OP_LDR.U(OP_W)

  // Instruction Types for chisel
  val TYPE_W = DEC_LITS.TYPE_W.W
  def I_T = UInt(TYPE_W)
  val I_X = DEC_LITS.I_X.U(TYPE_W)
  val I_LogSR = DEC_LITS.I_LogSR.U(TYPE_W)
  val I_LogI  = DEC_LITS.I_LogI.U(TYPE_W)
  val I_MovI  = DEC_LITS.I_MovI.U(TYPE_W)
  val I_BitF  = DEC_LITS.I_BitF.U(TYPE_W)
  val I_BImm  = DEC_LITS.I_BImm.U(TYPE_W)
  val I_BCImm = DEC_LITS.I_BCImm.U(TYPE_W)
  val I_ASSR  = DEC_LITS.I_ASSR.U(TYPE_W)
  val I_ASImm = DEC_LITS.I_ASImm.U(TYPE_W)
  val I_LSImm = DEC_LITS.I_LSImm.U(TYPE_W)
  val I_CSel  = DEC_LITS.I_CSel.U(TYPE_W)
  val I_CCImm = DEC_LITS.I_CCImm.U(TYPE_W)
  val I_CCReg = DEC_LITS.I_CCReg.U(TYPE_W)
  val I_CBImm = DEC_LITS.I_CBImm.U(TYPE_W)
  val I_PCRel = DEC_LITS.I_PCRel.U(TYPE_W)
}

object DECODE_MATCHING_TABLES
{
  import INSTRUCTIONS._

  def decode_default: List[UInt] =
    //
    //              RD
    //  IN TR       EN
    //  TYPE        | RS1
    //    |         |  EN        COND
    //    |         |  |           EN
    //    |         |  |           | NZCV
    //    |    INST |  | RS2  SHIFT|  EN
    //    |     OP  |  |  EN    EN |  |
    //    |     |   |  |  | IMM |  |  |
    //    |     |   |  |  |  EN |  |  |
    //    |     |   |  |  |  |  |  |  |
    //    |     |   |  |  |  |  |  |  |
    //    |     |   |  |  |  |  |  |  |
    List(I_X, OP_X, N, N, N, N, N, N, N)

  def decode_table: Array[(BitPat, List[UInt])]  =
    Array(
      //
      //                                      RD
      //                    IN TR             EN
      //                    TYPE              | RS1
      //                      |               |  EN        COND
      //                      |               |  |           EN
      //                      |               |  |           | NZCV
      //                      |      INST     |  | RS2  SHIFT|  EN
      //                      |       OP      |  |  EN    EN |  |
      //                      |       |       |  |  | IMM |  |  |
      //                      |       |       |  |  |  EN |  |  |
      //                      |       |       |  |  |  |  |  |  |
      //                      |       |       |  |  |  |  |  |  |
      //                      |       |       |  |  |  |  |  |  |
      // PC-Rel
      PCRel_ADR    -> List(I_PCRel, OP_ADR,   Y, N, N, Y, N, N, N),
      PCRel_ADRP   -> List(I_PCRel, OP_ADRP,  Y, N, N, Y, N, N, N),
      // Logical (shifted register)
      LogSR_AND    -> List(I_LogSR, OP_AND,   Y, Y, Y, N, Y, N, N),
      LogSR_BIC    -> List(I_LogSR, OP_BIC,   Y, Y, Y, N, Y, N, N),
      LogSR_ORR    -> List(I_LogSR, OP_ORR,   Y, Y, Y, N, Y, N, N),
      LogSR_ORN    -> List(I_LogSR, OP_ORN,   Y, Y, Y, N, Y, N, N),
      LogSR_EOR    -> List(I_LogSR, OP_EOR,   Y, Y, Y, N, Y, N, N),
      LogSR_EON    -> List(I_LogSR, OP_EON,   Y, Y, Y, N, Y, N, N),
      LogSR_ANDS   -> List(I_LogSR, OP_AND,   Y, Y, Y, N, Y, N, Y),
      LogSR_BICS   -> List(I_LogSR, OP_BIC,   Y, Y, Y, N, Y, N, Y),
      // Logical (immediate)
      LogI_AND     -> List(I_LogI,  OP_AND,   Y, Y, N, Y, N, N, N),
      LogI_ORR     -> List(I_LogI,  OP_ORR,   Y, Y, N, Y, N, N, N),
      LogI_EOR     -> List(I_LogI,  OP_EOR,   Y, Y, N, Y, N, N, N),
      LogI_ANDS    -> List(I_LogI,  OP_AND,   Y, Y, N, Y, N, N, Y),
      // Move wide (immediate)
      MovI_MOVN     -> List(I_MovI, OP_MOVN,  Y, N, Y, Y, N, N, N),
      MovI_MOVZ     -> List(I_MovI, OP_MOVZ,  Y, N, Y, Y, N, N, N),
      MovI_MOVK     -> List(I_MovI, OP_MOVK,  Y, N, Y, Y, N, N, N),
      // Bitfield
      BitF_SBFM    -> List(I_BitF,  OP_SBFM,  Y, N, Y, Y, N, N, N),
      BitF_BFM     -> List(I_BitF,  OP_BFM,   Y, N, Y, Y, N, N, N),
      BitF_UBFM    -> List(I_BitF,  OP_UBFM,  Y, N, Y, Y, N, N, N),
      // Conditional select
      CSel_CSEL    -> List(I_CSel,  OP_CSEL,  Y, Y, Y, N, N, Y, N),
      CSel_CSINC   -> List(I_CSel,  OP_CSINC, Y, Y, Y, N, N, Y, N),
      CSel_CSINV   -> List(I_CSel,  OP_CSINV, Y, Y, Y, N, N, Y, N),
      CSel_CSNEG   -> List(I_CSel,  OP_CSNEG, Y, Y, Y, N, N, Y, N),
      // Conditional compare (immediate)
      CCImm_CCMN   -> List(I_CCImm, OP_CCMN,  N, N, Y, Y, N, Y, Y),
      CCImm_CCMP   -> List(I_CCImm, OP_CCMP,  N, N, Y, Y, N, Y, Y),
      // Conditional compare (register)
      CCReg_CCMN   -> List(I_CCReg, OP_CCMN,  N, Y, Y, N, N, Y, Y),
      CCReg_CCMP   -> List(I_CCReg, OP_CCMP,  N, Y, Y, N, N, Y, Y),
      // Add/subtract (shifted register)
      ASSR_ADD     -> List(I_ASSR,  OP_ADD,   Y, Y, Y, N, Y, N, N),
      ASSR_ADDS    -> List(I_ASSR,  OP_ADD,   Y, Y, Y, N, Y, N, Y),
      ASSR_SUB     -> List(I_ASSR,  OP_SUB,   Y, Y, Y, N, Y, N, N),
      ASSR_SUBS    -> List(I_ASSR,  OP_SUB,   Y, Y, Y, N, Y, N, Y),
      // Add/subtract (immediate)
      ASImm_ADD_I  -> List(I_ASImm, OP_ADD,   Y, Y, N, Y, Y, N, N),
      ASImm_ADDS_I -> List(I_ASImm, OP_ADD,   Y, Y, N, Y, Y, N, Y),
      ASImm_SUB_I  -> List(I_ASImm, OP_SUB,   Y, Y, N, Y, Y, N, N),
      ASImm_SUBS_I -> List(I_ASImm, OP_SUB,   Y, Y, N, Y, Y, N, Y),
      // Unconditional branch (immediate)
      BImm_B       -> List(I_BImm,  OP_B,     N, N, N, Y, N, N, N),
      BImm_BL      -> List(I_BImm,  OP_BL,    N, N, N, Y, N, N, N),
      // Conditional branch (immediate)
      BCond        -> List(I_BCImm, OP_BCOND, N, N, N, Y, N, Y, N),
      // Compare and branch (immediate)
      CBImm_CBZ    -> List(I_CBImm, OP_CBZ,   N, N, Y, Y, N, N, N),
      CBImm_CBNZ   -> List(I_CBImm, OP_CBNZ,  N, N, Y, Y, N, N, N),
      // load/store (immediate)
      LDR_I        -> List(I_LSImm, OP_LDR,   Y, N, N, Y, N, N, N),
    )
}

