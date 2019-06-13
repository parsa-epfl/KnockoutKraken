package protoflex

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import utils.{AssemblyParser, AssemblyInstruction, DInstExtractor}
import common.DEC_LITS._

class ExecuteALULog_SR(c: ExecuteUnit) extends PeekPokeTester(c)
{
  val instrs = AssemblyParser.parse("alu.x")
  val rVal1 = 1337
  val rVal2 = 79271

  val dinst_in = DInstExtractor(c)

  instrs map {
    case inst if inst.inst_en.toInt == N =>
      println("Line : " + inst.line)
      println("     - Skipped : Instruction not decoded")
    case inst if inst.op.toInt == OP_EON  || inst.op.toInt == OP_ORN || inst.op.toInt == OP_SUB =>
      println("Line : " + inst.line )
      println("     - Skipped : Negative scala operations don't match hardware results,"
              + "\n scala doesn't have unsigned types")
    case inst if inst.inst_en.toInt != N =>
      println("Line : " + inst.line)
      dinst_in zip inst.io map { case (io, value) => poke(io, value)}
      poke(c.io.rVal1, rVal1)
      poke(c.io.rVal2, rVal2)

      //val val2 = if(inst.getSig("imm_en") == 0) rVal2 else inst.getSig("imm").toInt
      val val2 = if(inst.shift_en == Y ) {
        (inst.shift_type.toInt, inst.imm.toInt) match {
          case (LSL, imm)  => rVal2 <<  imm
          case (LSR, imm)  => rVal2 >>> imm
          case (ASR, imm)  => rVal2 >>  imm
          case (ROR, imm)  => rVal2 >>> imm | rVal2 << ~imm
          case (_,_) => println("    -- Mismatched shift"); 0
        }
      } else { rVal2 }

      val res = inst.op.toInt match {
        case OP_AND => (rVal1  &  val2)
        case OP_BIC => (rVal1  & ~val2)
        case OP_ORR => (rVal1  |  val2)
        case OP_ORN => (rVal1  | ~val2)
        case OP_EOR => (rVal1  ^  val2)
        case OP_EON => (rVal1  ^ ~val2)
        case OP_ADD => (rVal1  +  val2)
        case OP_SUB => (rVal1  -  val2)
      }

      expect(c.io.einst.bits.res, res)
      expect(c.io.einst.valid, 1)
      step(1)
  }
}

class ExecuteLog_SRCond(c: ExecuteUnit) extends PeekPokeTester(c)
{
  // Pokes ANDS instruction
  val dinst_in = DInstExtractor(c)
  val andsInst = AssemblyInstruction(0, 0, 0)
  dinst_in zip andsInst.io map { case (io, value) => poke(io, value) }
  expect(c.io.einst.bits.nzcv_en, 1)

  def nzcv_check(rVal1: BigInt, rVal2: BigInt, nzcv: BigInt) = {
    poke(c.io.rVal1, rVal1)
    poke(c.io.rVal2, rVal2)
    step(1)
    expect(c.io.einst.bits.nzcv, nzcv)
    expect(c.io.einst.valid, 1)
  }

  // Checks for n flag ( res < 0 )
  var nzcv = BigInt(Seq(1, 0, 1, 0).reverse.mkString, 2)
  var rVal1 = BigInt("8000000000000000", 16)
  var rVal2 = BigInt("8000000000000000", 16)
  nzcv_check(rVal1, rVal2, nzcv)


  //// Checks for z flag ( res == 0 )
  nzcv = BigInt(Seq(0, 1, 0, 0).reverse.mkString, 2)
  rVal1 = BigInt("0000101010101000", 16)
  rVal2 = BigInt("0001010101010100", 16)
  nzcv_check(rVal1, rVal2, nzcv)

  // Checks for c flag ( a + b : unsigned overflow )
  nzcv = BigInt(Seq(0, 0, 1, 0).reverse.mkString, 2)
  rVal1 = BigInt("FFFFFFFFFFFFFFFF", 16)
  rVal2 = BigInt("0000000000000001", 16)
  nzcv_check(rVal1, rVal2, nzcv)

  // Checks for v flag ( a + b : signed overflow )
  // Positive overflow
  nzcv = BigInt(Seq(0, 0, 0, 1).reverse.mkString, 2)
  rVal1 = BigInt("7FFFFFFFFFFFFFFF", 16)
  rVal2 = BigInt("0000000000000001", 16)
  nzcv_check(rVal1, rVal2, nzcv)

  // negative overflow
  nzcv = BigInt(Seq(1, 0, 1, 1).reverse.mkString, 2)
  rVal1 = BigInt("FFFFFFFFFFFFFFFF", 16)
  rVal2 = BigInt("8000000000000000", 16)
  nzcv_check(rVal1, rVal2, nzcv)

}


class ExecuteTester extends ChiselFlatSpec
{
  behavior of "Executer"


  backends foreach { backend =>
    it should s"execute ALU ops and shifts for dual register operations (with $backend)" in {
      Driver(() => new ExecuteUnit, backend)((c) => new ExecuteALULog_SR(c)) should be (true)
    }
  }

  backends foreach { backend =>
    it should s"check for nzcv flag for dual register operations (with $backend)" in {
      Driver(() => new ExecuteUnit, backend)((c) => new ExecuteLog_SRCond(c)) should be (true)
    }
  }
}

object ExecuteRepl extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new ExecuteUnit)
}

