package arm

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

  // Unconditonal branch (immediate)
  val OP_B = DEC_LITS.OP_B.U(OP_W)
  val OP_BL = DEC_LITS.OP_BL.U(OP_W)

  // Conditional branch (immediate)
  val OP_BCOND = DEC_LITS.OP_BCOND.U(OP_W)

  // Uncondional branch (register)
  val OP_BR  = DEC_LITS.OP_BR.U(OP_W)
  val OP_BLR = DEC_LITS.OP_BLR.U(OP_W)
  val OP_RET = DEC_LITS.OP_RET.U(OP_W)

  // Test and branch (immediate)
  val OP_TBZ  = DEC_LITS.OP_TBZ.U(OP_W)
  val OP_TBNZ = DEC_LITS.OP_TBNZ.U(OP_W)

  // Conditional Branch
  val OP_CBZ  = DEC_LITS.OP_CBZ.U(OP_W)
  val OP_CBNZ = DEC_LITS.OP_CBNZ.U(OP_W)

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

  // Data-processing (1 source)
  val OP_RBIT  = DEC_LITS.OP_RBIT.U(OP_W)
  val OP_REV16 = DEC_LITS.OP_REV16.U(OP_W)
  val OP_REV32 = DEC_LITS.OP_REV32.U(OP_W)
  val OP_REV   = DEC_LITS.OP_REV.U(OP_W)
  val OP_CLZ   = DEC_LITS.OP_CLZ.U(OP_W)
  val OP_CLS   = DEC_LITS.OP_CLS.U(OP_W)

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

  val SIZEB  = DEC_LITS.SIZEB.U(2.W)
  val SIZEH  = DEC_LITS.SIZEH.U(2.W)
  val SIZE32 = DEC_LITS.SIZE32.U(2.W)
  val SIZE64 = DEC_LITS.SIZE64.U(2.W)

  val OP_STRB   = DEC_LITS.OP_STRB.U(OP_W)
  val OP_STRH   = DEC_LITS.OP_STRH.U(OP_W)
  val OP_STR32  = DEC_LITS.OP_STR32.U(OP_W)
  val OP_STR64  = DEC_LITS.OP_STR64.U(OP_W)
  val OP_LDRB   = DEC_LITS.OP_LDRB.U(OP_W)
  val OP_LDRH   = DEC_LITS.OP_LDRH.U(OP_W)
  val OP_LDR32  = DEC_LITS.OP_LDR32.U(OP_W)
  val OP_LDR64  = DEC_LITS.OP_LDR64.U(OP_W)

  val OP_LDRSB  = DEC_LITS.OP_LDRSB.U(OP_W)
  val OP_LDRSH  = DEC_LITS.OP_LDRSH.U(OP_W)
  val OP_LDRSW  = DEC_LITS.OP_LDRSW.U(OP_W)

  val OP_STURB  = DEC_LITS.OP_STURB.U(OP_W)
  val OP_STURH  = DEC_LITS.OP_STURH.U(OP_W)
  val OP_STUR32 = DEC_LITS.OP_STUR32.U(OP_W)
  val OP_STUR64 = DEC_LITS.OP_STUR64.U(OP_W)
  val OP_LDURB  = DEC_LITS.OP_LDURB.U(OP_W)
  val OP_LDURH  = DEC_LITS.OP_LDURH.U(OP_W)
  val OP_LDUR32 = DEC_LITS.OP_LDUR32.U(OP_W)
  val OP_LDUR64 = DEC_LITS.OP_LDUR64.U(OP_W)

  val OP_LDURSB  = DEC_LITS.OP_LDURSB.U(OP_W)
  val OP_LDURSH  = DEC_LITS.OP_LDURSH.U(OP_W)
  val OP_LDURSW  = DEC_LITS.OP_LDURSW.U(OP_W)

  val OP_STP32  = DEC_LITS.OP_STP32.U(OP_W)
  val OP_STP64  = DEC_LITS.OP_STP64.U(OP_W)
  val OP_LDP32  = DEC_LITS.OP_LDP32.U(OP_W)
  val OP_LDP64  = DEC_LITS.OP_LDP64.U(OP_W)

  // Instruction Types for chisel
  val TYPE_W = DEC_LITS.TYPE_W.W
  def I_T = UInt(TYPE_W)

  val I_X = DEC_LITS.I_X.U(TYPE_W)
  val I_LogSR =  DEC_LITS.I_LogSR.U(TYPE_W) // Logical (shifted register)
  val I_LogI  =  DEC_LITS.I_LogI.U(TYPE_W) // Logical (immediate)
  val I_BitF  =  DEC_LITS.I_BitF.U(TYPE_W) // Logical (shifted register)

  val I_DP1S  =  DEC_LITS.I_DP1S.U(TYPE_W) // Data-processing (1 source)
  val I_DP2S  =  DEC_LITS.I_DP2S.U(TYPE_W) // Data-processing (2 source)
  val I_CCImm =  DEC_LITS.I_CCImm.U(TYPE_W) // Conditional compare (immediate)
  val I_CCReg =  DEC_LITS.I_CCReg.U(TYPE_W) // Conditional compare (register)

  val I_ASImm =  DEC_LITS.I_ASImm.U(TYPE_W) // Add/Subtract (Immediate)
  val I_ASSR  =  DEC_LITS.I_ASSR.U(TYPE_W) // Add/subtract (shifted register)
  val I_MovI  =  DEC_LITS.I_MovI.U(TYPE_W) // Move wide (immediate)
  val I_CSel  =  DEC_LITS.I_CSel.U(TYPE_W) // Conditional select

  val I_BImm  =  DEC_LITS.I_BImm.U(TYPE_W)  // Unconditional branch (immediate)
  val I_BCImm =  DEC_LITS.I_BCImm.U(TYPE_W) // Conditional branch (immediate)
  val I_BReg  =  DEC_LITS.I_BReg.U(TYPE_W)  // Conditional branch (register)
  val I_CBImm =  DEC_LITS.I_CBImm.U(TYPE_W) // Branch and Compare (immediate)

  val I_LSRReg  = DEC_LITS.I_LSRReg.U(TYPE_W)  // Load/store register (register offset)
  val I_LSPoReg = DEC_LITS.I_LSPoReg.U(TYPE_W) // Load/store register (post-indexed)
  val I_LSUReg  = DEC_LITS.I_LSUReg.U(TYPE_W)  // Load/store register (unscaled immediate)
  val I_LSPrReg = DEC_LITS.I_LSPrReg.U(TYPE_W) // Load/store register (pre-indexed)

  val I_LSUImm = DEC_LITS.I_LSUImm.U(TYPE_W) // Load/store (unsigned immediate)
  val I_LSPPoReg = DEC_LITS.I_LSPoReg.U(TYPE_W) // Load/store pair register (post-indexed)
  val I_LSPReg   = DEC_LITS.I_LSPReg.U(TYPE_W)  // Load/store pair register (signed offset)
  val I_LSPPrReg = DEC_LITS.I_LSPrReg.U(TYPE_W) // Load/store pair register (pre-indexed)

  val I_TBImm =  DEC_LITS.I_TBImm.U(TYPE_W) // Test and branch (immediate)
  val I_PCRel =  DEC_LITS.I_PCRel.U(TYPE_W) // PC-Relative
}

object DECODE_MATCHING_TABLES
{
  import INSTRUCTIONS._

  def decode_default: List[UInt] =
    //
    //              RD
    //  IN TR       EN
    //  TYPE        |
    //    |         |    COND
    //    |         |      EN
    //    |         |      | NZCV
    //    |    INST | SHIFT|  EN
    //    |     OP  |  EN  |  |
    //    |     |   |  |   |  |32BIT
    //    |     |   |  |   |  |  |
    //    |     |   |  |   |  |  |
    //    |     |   |  |   |  |  |
    //    |     |   |  |   |  |  |
    List(I_X, OP_X, N, N,  N, N, N)

  def decode_table: Array[(BitPat, List[UInt])]  =
    Array(
      //
      //
      //                    INSTR
      //                    TYPE             RD
      //                      |              EN   COND
      //                      |               |     EN
      //                      |               |     | NZCV
      //                      |      INST     |SHIFT|  EN
      //                      |       OP      |  EN |  |
      //                      |       |       |  |  |  |32BIT
      //                      |       |       |  |  |  |  |
      //                      |       |       |  |  |  |  |
      //                      |       |       |  |  |  |  |
      // PC-Rel
      PCRel_ADR    -> List(I_PCRel, OP_ADR,   Y, N, N, N, N),
      PCRel_ADRP   -> List(I_PCRel, OP_ADRP,  Y, N, N, N, N),
      // Logical (shifted register)
      LogSR_TST32  -> List(I_LogSR, OP_AND,   N, Y, N, Y, Y),
      LogSR_AND32  -> List(I_LogSR, OP_AND,   Y, Y, N, N, Y),
      LogSR_BIC32  -> List(I_LogSR, OP_BIC,   Y, Y, N, N, Y),
      LogSR_ORR32  -> List(I_LogSR, OP_ORR,   Y, Y, N, N, Y),
      LogSR_ORN32  -> List(I_LogSR, OP_ORN,   Y, Y, N, N, Y),
      LogSR_EOR32  -> List(I_LogSR, OP_EOR,   Y, Y, N, N, Y),
      LogSR_EON32  -> List(I_LogSR, OP_EON,   Y, Y, N, N, Y),
      LogSR_ANDS32 -> List(I_LogSR, OP_AND,   Y, Y, N, Y, Y),
      LogSR_BICS32 -> List(I_LogSR, OP_BIC,   Y, Y, N, Y, Y),
      LogSR_TST    -> List(I_LogSR, OP_AND,   N, Y, N, Y, N),
      LogSR_AND    -> List(I_LogSR, OP_AND,   Y, Y, N, N, N),
      LogSR_BIC    -> List(I_LogSR, OP_BIC,   Y, Y, N, N, N),
      LogSR_ORR    -> List(I_LogSR, OP_ORR,   Y, Y, N, N, N),
      LogSR_ORN    -> List(I_LogSR, OP_ORN,   Y, Y, N, N, N),
      LogSR_EOR    -> List(I_LogSR, OP_EOR,   Y, Y, N, N, N),
      LogSR_EON    -> List(I_LogSR, OP_EON,   Y, Y, N, N, N),
      LogSR_ANDS   -> List(I_LogSR, OP_AND,   Y, Y, N, Y, N),
      LogSR_BICS   -> List(I_LogSR, OP_BIC,   Y, Y, N, Y, N),
      // Logical (immediate)
      LogI_TST32   -> List(I_LogI,  OP_AND,   N, N, N, Y, Y),
      LogI_AND32   -> List(I_LogI,  OP_AND,   Y, N, N, N, Y),
      LogI_ORR32   -> List(I_LogI,  OP_ORR,   Y, N, N, N, Y),
      LogI_EOR32   -> List(I_LogI,  OP_EOR,   Y, N, N, N, Y),
      LogI_ANDS32  -> List(I_LogI,  OP_AND,   Y, N, N, Y, Y),
      LogI_TST     -> List(I_LogI,  OP_AND,   N, N, N, Y, N),
      LogI_AND     -> List(I_LogI,  OP_AND,   Y, N, N, N, N),
      LogI_ORR     -> List(I_LogI,  OP_ORR,   Y, N, N, N, N),
      LogI_EOR     -> List(I_LogI,  OP_EOR,   Y, N, N, N, N),
      LogI_ANDS    -> List(I_LogI,  OP_AND,   Y, N, N, Y, N),
      // Move wide (immediate)
      MovI_MOVN32  -> List(I_MovI, OP_MOVN,   Y, N, N, N, Y),
      MovI_MOVZ32  -> List(I_MovI, OP_MOVZ,   Y, N, N, N, Y),
      MovI_MOVK32  -> List(I_MovI, OP_MOVK,   Y, N, N, N, Y),
      MovI_MOVN    -> List(I_MovI, OP_MOVN,   Y, N, N, N, N),
      MovI_MOVZ    -> List(I_MovI, OP_MOVZ,   Y, N, N, N, N),
      MovI_MOVK    -> List(I_MovI, OP_MOVK,   Y, N, N, N, N),
      // Bitfield
      BitF_SBFM32  -> List(I_BitF,  OP_SBFM,  Y, Y, N, N, Y),
      BitF_BFM32   -> List(I_BitF,  OP_BFM,   Y, Y, N, N, Y),
      BitF_UBFM32  -> List(I_BitF,  OP_UBFM,  Y, Y, N, N, Y),
      BitF_SBFM    -> List(I_BitF,  OP_SBFM,  Y, Y, N, N, N),
      BitF_BFM     -> List(I_BitF,  OP_BFM,   Y, Y, N, N, N),
      BitF_UBFM    -> List(I_BitF,  OP_UBFM,  Y, Y, N, N, N),
      // Conditional select
      CSel_CSEL32  -> List(I_CSel,  OP_CSEL,  Y, N, Y, N, Y),
      CSel_CSINC32 -> List(I_CSel,  OP_CSINC, Y, N, Y, N, Y),
      CSel_CSINV32 -> List(I_CSel,  OP_CSINV, Y, N, Y, N, Y),
      CSel_CSNEG32 -> List(I_CSel,  OP_CSNEG, Y, N, Y, N, Y),
      CSel_CSEL    -> List(I_CSel,  OP_CSEL,  Y, N, Y, N, N),
      CSel_CSINC   -> List(I_CSel,  OP_CSINC, Y, N, Y, N, N),
      CSel_CSINV   -> List(I_CSel,  OP_CSINV, Y, N, Y, N, N),
      CSel_CSNEG   -> List(I_CSel,  OP_CSNEG, Y, N, Y, N, N),
      // Conditional compare (immediate)
      CCImm_CCMN   -> List(I_CCImm, OP_CCMN,  N, N, Y, Y, N),
      CCImm_CCMP   -> List(I_CCImm, OP_CCMP,  N, N, Y, Y, N),
      CCImm_CCMP32 -> List(I_CCImm, OP_CCMP,  N, N, Y, Y, Y),
      // Conditional compare (register)
      CCReg_CCMN   -> List(I_CCReg, OP_CCMN,  N, N, Y, Y, N),
      CCReg_CCMP   -> List(I_CCReg, OP_CCMP,  N, N, Y, Y, N),
      CCReg_CCMP32 -> List(I_CCReg, OP_CCMP,  N, N, Y, Y, Y),
      // Add/subtract (shifted register)
      ASSR_CMP32   -> List(I_ASSR,  OP_SUB,   N, Y, N, Y, Y),
      ASSR_CMN32   -> List(I_ASSR,  OP_ADD,   N, Y, N, Y, Y),
      ASSR_ADD32   -> List(I_ASSR,  OP_ADD,   Y, Y, N, N, Y),
      ASSR_ADDS32  -> List(I_ASSR,  OP_ADD,   Y, Y, N, Y, Y),
      ASSR_SUB32   -> List(I_ASSR,  OP_SUB,   Y, Y, N, N, Y),
      ASSR_SUBS32  -> List(I_ASSR,  OP_SUB,   Y, Y, N, Y, Y),
      ASSR_CMP     -> List(I_ASSR,  OP_SUB,   N, Y, N, Y, N),
      ASSR_CMN     -> List(I_ASSR,  OP_ADD,   N, Y, N, Y, N),
      ASSR_ADD     -> List(I_ASSR,  OP_ADD,   Y, Y, N, N, N),
      ASSR_ADDS    -> List(I_ASSR,  OP_ADD,   Y, Y, N, Y, N),
      ASSR_SUB     -> List(I_ASSR,  OP_SUB,   Y, Y, N, N, N),
      ASSR_SUBS    -> List(I_ASSR,  OP_SUB,   Y, Y, N, Y, N),
      // Add/subtract (immediate)
      ASImm_CMP32  -> List(I_ASImm, OP_SUB,   N, Y, N, Y, Y),
      ASImm_CMN32  -> List(I_ASImm, OP_ADD,   N, Y, N, Y, Y),
      ASImm_ADD32  -> List(I_ASImm, OP_ADD,   Y, Y, N, N, Y),
      ASImm_ADDS32 -> List(I_ASImm, OP_ADD,   Y, Y, N, Y, Y),
      ASImm_SUB32  -> List(I_ASImm, OP_SUB,   Y, Y, N, N, Y),
      ASImm_SUBS32 -> List(I_ASImm, OP_SUB,   Y, Y, N, Y, Y),
      ASImm_CMP    -> List(I_ASImm, OP_SUB,   N, Y, N, Y, N),
      ASImm_CMN    -> List(I_ASImm, OP_ADD,   N, Y, N, Y, N),
      ASImm_ADD    -> List(I_ASImm, OP_ADD,   Y, Y, N, N, N),
      ASImm_ADDS   -> List(I_ASImm, OP_ADD,   Y, Y, N, Y, N),
      ASImm_SUB    -> List(I_ASImm, OP_SUB,   Y, Y, N, N, N),
      ASImm_SUBS   -> List(I_ASImm, OP_SUB,   Y, Y, N, Y, N),
      // Data-processing (1 source)
      DP1S_RBIT32  -> List(I_DP1S,  OP_RBIT,  Y, N, N, N, Y),
      DP1S_32REV16 -> List(I_DP1S,  OP_REV16, Y, N, N, N, Y),
      DP1S_32REV32 -> List(I_DP1S,  OP_REV32, Y, N, N, N, Y),
      DP1S_CLZ32   -> List(I_DP1S,  OP_CLZ,   Y, N, N, N, Y),
      DP1S_CLS32   -> List(I_DP1S,  OP_CLS,   Y, N, N, N, Y),
      DP1S_RBIT    -> List(I_DP1S,  OP_RBIT,  Y, N, N, N, N),
      DP1S_REV16   -> List(I_DP1S,  OP_REV16, Y, N, N, N, N),
      DP1S_REV32   -> List(I_DP1S,  OP_REV32, Y, N, N, N, N),
      DP1S_REV     -> List(I_DP1S,  OP_REV,   Y, N, N, N, N),
      DP1S_CLZ     -> List(I_DP1S,  OP_CLZ,   Y, N, N, N, N),
      DP1S_CLS     -> List(I_DP1S,  OP_CLS,   Y, N, N, N, N),
      // Data-processing (2 sources)
      DP2S_LSLV32  -> List(I_DP2S,  LSL,      Y, Y, N, N, Y),
      DP2S_LSRV32  -> List(I_DP2S,  LSR,      Y, Y, N, N, Y),
      DP2S_ASRV32  -> List(I_DP2S,  ASR,      Y, Y, N, N, Y),
      DP2S_RORV32  -> List(I_DP2S,  ROR,      Y, Y, N, N, Y),
      DP2S_LSLV    -> List(I_DP2S,  LSL,      Y, Y, N, N, N),
      DP2S_LSRV    -> List(I_DP2S,  LSR,      Y, Y, N, N, N),
      DP2S_ASRV    -> List(I_DP2S,  ASR,      Y, Y, N, N, N),
      DP2S_RORV    -> List(I_DP2S,  ROR,      Y, Y, N, N, N),
      // Unconditional branch (immediate)
      BImm_B       -> List(I_BImm,  OP_B,     N, N, N, N, N),
      BImm_BL      -> List(I_BImm,  OP_BL,    N, N, N, N, N),
      // Unconditional branch (register)
      BReg_BR      -> List(I_BReg,  OP_BR,    N, N, N, N, N),
      BReg_BLR     -> List(I_BReg,  OP_BLR,   N, N, N, N, N),
      BReg_RET     -> List(I_BReg,  OP_RET,   N, N, N, N, N),
      // Test and branch (immediate)
      TBImm_TBZ32  -> List(I_TBImm, OP_TBZ,   N, N, N, N, Y),
      TBImm_TBNZ32 -> List(I_TBImm, OP_TBNZ,  N, N, N, N, Y),
      TBImm_TBZ    -> List(I_TBImm, OP_TBZ,   N, N, N, N, N),
      TBImm_TBNZ   -> List(I_TBImm, OP_TBNZ,  N, N, N, N, N),
      // Conditional branch (immediate)
      BCond        -> List(I_BCImm, OP_BCOND, N, N, Y, N, N),
      // Compare and branch (immediate)
      CBImm_CBZ32  -> List(I_CBImm, OP_CBZ,   N, N, N, N, Y),
      CBImm_CBNZ32 -> List(I_CBImm, OP_CBNZ,  N, N, N, N, Y),
      CBImm_CBZ    -> List(I_CBImm, OP_CBZ,   N, N, N, N, N),
      CBImm_CBNZ   -> List(I_CBImm, OP_CBNZ,  N, N, N, N, N),
      // I_TYPE_OP -> List(I_TYPE,   OP_OP,    RD, SHIFT, COND, NZCV, IS32BIT)
      // Load/store pair register (post-indexed)
      LSPPoReg_STP32 -> List(I_LSPPoReg, OP_STP32,   Y, N, N, N, Y),
      LSPPoReg_STP64 -> List(I_LSPPoReg, OP_STP64,   Y, N, N, N, N),
      LSPPoReg_LDP32 -> List(I_LSPPoReg, OP_LDP32,   Y, N, N, N, Y),
      LSPPoReg_LDP64 -> List(I_LSPPoReg, OP_LDP64,   Y, N, N, N, N),
      // Load/store pair register (signed offset)
      LSPReg_STP32 -> List(I_LSPReg, OP_STP32,   Y, N, N, N, Y),
      LSPReg_STP64 -> List(I_LSPReg, OP_STP64,   Y, N, N, N, N),
      LSPReg_LDP32 -> List(I_LSPReg, OP_LDP32,   Y, N, N, N, Y),
      LSPReg_LDP64 -> List(I_LSPReg, OP_LDP64,   Y, N, N, N, N),
      // Load/store pair register (pre-indexed)
      LSPPrReg_STP32 -> List(I_LSPPrReg, OP_STP32,   Y, N, N, N, Y),
      LSPPrReg_STP64 -> List(I_LSPPrReg, OP_STP64,   Y, N, N, N, N),
      LSPPrReg_LDP32 -> List(I_LSPPrReg, OP_LDP32,   Y, N, N, N, Y),
      LSPPrReg_LDP64 -> List(I_LSPPrReg, OP_LDP64,   Y, N, N, N, N),
      // Load/store register (register offset)
      LSRReg_STRB  -> List(I_LSRReg, OP_STRB,    Y, N, N, N, Y),
      LSRReg_STRH  -> List(I_LSRReg, OP_STRH,    Y, N, N, N, Y),
      LSRReg_STR32 -> List(I_LSRReg, OP_STR32,   Y, N, N, N, Y),
      LSRReg_STR64 -> List(I_LSRReg, OP_STR64,   Y, N, N, N, N),
      LSRReg_LDRB  -> List(I_LSRReg, OP_LDRB,    Y, N, N, N, Y),
      LSRReg_LDRH  -> List(I_LSRReg, OP_LDRH,    Y, N, N, N, Y),
      LSRReg_LDR32 -> List(I_LSRReg, OP_LDR32,   Y, N, N, N, Y),
      LSRReg_LDR64 -> List(I_LSRReg, OP_LDR64,   Y, N, N, N, N),
      // Load/store register (unsigned immediate)
      LSUImm_STRB  -> List(I_LSUImm, OP_STRB,    Y, N, N, N, Y),
      LSUImm_STRH  -> List(I_LSUImm, OP_STRH,    Y, N, N, N, Y),
      LSUImm_STR32 -> List(I_LSUImm, OP_STR32,   Y, N, N, N, Y),
      LSUImm_STR64 -> List(I_LSUImm, OP_STR64,   Y, N, N, N, N),
      LSUImm_LDRB  -> List(I_LSUImm, OP_LDRB,    Y, N, N, N, Y),
      LSUImm_LDRH  -> List(I_LSUImm, OP_LDRH,    Y, N, N, N, Y),
      LSUImm_LDR32 -> List(I_LSUImm, OP_LDR32,   Y, N, N, N, Y),
      LSUImm_LDR64 -> List(I_LSUImm, OP_LDR64,   Y, N, N, N, N),
      LSUImm_LDRSW -> List(I_LSUImm, OP_LDRSW,   Y, N, N, N, N),
      LSUImm_LDRSB64 -> List(I_LSUImm, OP_LDRSB, Y, N, N, N, N),
      LSUImm_LDRSH64 -> List(I_LSUImm, OP_LDRSH, Y, N, N, N, N),
      // Load/store register (unscaled immediate)
      LSUReg_STURB    -> List(I_LSUReg, OP_STURB,  Y, N, N, N, N),
      LSUReg_LDURB    -> List(I_LSUReg, OP_LDURB,  Y, N, N, N, N),
      LSUReg_LDURSB   -> List(I_LSUReg, OP_LDURSB, Y, N, N, N, N),
      LSUReg_LDURSB32 -> List(I_LSUReg, OP_LDURSB, Y, N, N, N, Y),
      LSUReg_STURH    -> List(I_LSUReg, OP_STURH,  Y, N, N, N, N),
      LSUReg_LDURH    -> List(I_LSUReg, OP_LDURH,  Y, N, N, N, N),
      LSUReg_LDURSH   -> List(I_LSUReg, OP_LDURSH, Y, N, N, N, N),
      LSUReg_LDURSH32 -> List(I_LSUReg, OP_LDURSH, Y, N, N, N, Y),
      LSUReg_STUR32   -> List(I_LSUReg, OP_STUR32, Y, N, N, N, Y),
      LSUReg_LDUR32   -> List(I_LSUReg, OP_LDUR32, Y, N, N, N, Y),
      LSUReg_LDURSW   -> List(I_LSUReg, OP_LDURSW, Y, N, N, N, N),
      LSUReg_STUR     -> List(I_LSUReg, OP_STUR64, Y, N, N, N, N),
      LSUReg_LDUR     -> List(I_LSUReg, OP_LDUR64, Y, N, N, N, N),
      // Load/store register (post-indexed)
      LSPoReg_STRB    -> List(I_LSPoReg, OP_STRB,  Y, N, N, N, N),
      LSPoReg_LDRB    -> List(I_LSPoReg, OP_LDRB,  Y, N, N, N, N),
      LSPoReg_LDRSB   -> List(I_LSPoReg, OP_LDRSB, Y, N, N, N, N),
      LSPoReg_LDRSB32 -> List(I_LSPoReg, OP_LDRSB, Y, N, N, N, Y),
      LSPoReg_STRH    -> List(I_LSPoReg, OP_STRH,  Y, N, N, N, N),
      LSPoReg_LDRH    -> List(I_LSPoReg, OP_LDRH,  Y, N, N, N, N),
      LSPoReg_LDRSH   -> List(I_LSPoReg, OP_LDRSH, Y, N, N, N, N),
      LSPoReg_LDRSH32 -> List(I_LSPoReg, OP_LDRSH, Y, N, N, N, Y),
      LSPoReg_STR32   -> List(I_LSPoReg, OP_STR32, Y, N, N, N, Y),
      LSPoReg_LDR32   -> List(I_LSPoReg, OP_LDR32, Y, N, N, N, Y),
      LSPoReg_LDRSW   -> List(I_LSPoReg, OP_LDRSW, Y, N, N, N, N),
      LSPoReg_STR     -> List(I_LSPoReg, OP_STR64, Y, N, N, N, N),
      LSPoReg_LDR     -> List(I_LSPoReg, OP_LDR64, Y, N, N, N, N),
      // Load/store register (pre-indexed)
      LSPrReg_STRB    -> List(I_LSPrReg, OP_STRB,  Y, N, N, N, N),
      LSPrReg_LDRB    -> List(I_LSPrReg, OP_LDRB,  Y, N, N, N, N),
      LSPrReg_LDRSB   -> List(I_LSPrReg, OP_LDRSB, Y, N, N, N, N),
      LSPrReg_LDRSB32 -> List(I_LSPrReg, OP_LDRSB, Y, N, N, N, Y),
      LSPrReg_STRH    -> List(I_LSPrReg, OP_STRH,  Y, N, N, N, N),
      LSPrReg_LDRH    -> List(I_LSPrReg, OP_LDRH,  Y, N, N, N, N),
      LSPrReg_LDRSH   -> List(I_LSPrReg, OP_LDRSH, Y, N, N, N, N),
      LSPrReg_LDRSH32 -> List(I_LSPrReg, OP_LDRSH, Y, N, N, N, Y),
      LSPrReg_STR32   -> List(I_LSPrReg, OP_STR32, Y, N, N, N, Y),
      LSPrReg_LDR32   -> List(I_LSPrReg, OP_LDR32, Y, N, N, N, Y),
      LSPrReg_LDRSW   -> List(I_LSPrReg, OP_LDRSW, Y, N, N, N, N),
      LSPrReg_STR     -> List(I_LSPrReg, OP_STR64, Y, N, N, N, N),
      LSPrReg_LDR     -> List(I_LSPrReg, OP_LDR64, Y, N, N, N, N)
   )
}

