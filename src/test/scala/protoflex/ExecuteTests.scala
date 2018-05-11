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
      println("Line : " + inst.line + " | Not decoded !")
    case inst if inst.op.toInt == OP_EON  || inst.op.toInt == OP_ORN || inst.op.toInt == OP_SUB =>
      println("Line : " + inst.line )
      println("     - Skipped : Negative scala operations don't match hardware results")
    case inst if inst.inst_en.toInt != N =>
      println("Line : " + inst.line)
      dinst_in zip inst.io map { case (io, value) => poke(io, value)}
      poke(c.io.rVal1, rVal1)
      poke(c.io.rVal2, rVal2)

      //val val2 = if(inst.getSig("imm_en") == 0) rVal2 else inst.getSig("imm").toInt
      val val2 = if(inst.shift_en != N ) {
        (inst.shift.toInt, inst.imm.toInt) match {
          case (LSL, imm)  => rVal2 <<  imm
          case (LSR, imm)  => rVal2 >>> imm
          case (ASR, imm)  => rVal2 >>  imm
          case (ROR, imm)  => rVal2 >>> imm | rVal2 << ~imm
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

      expect(c.io.einst.res, res)
      step(1)
  }
}

class ExecuteTester extends ChiselFlatSpec
{
  behavior of "Executer"

  backends foreach { backend =>
    it should s"execute ALU ops and shifts for dual register operations (with $backend)" in {
      Driver(() => new ExecuteUnit, backend)((c) => new ExecuteALULog_SR(c)) should be (true)
    }
  }
}

object ExecuteRepl extends App {
  iotesters.Driver.executeFirrtlRepl(args, () => new ExecuteUnit)
}

