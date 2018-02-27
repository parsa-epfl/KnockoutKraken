package trial

import chisel3._

object TYPES
{
  def TYPE_A = UInt(2.W)
}


class SObj extends Bundle
{
  val a = TYPES.TYPE_A
  val b = TYPES.TYPE_A
}

class TrialIO extends Bundle
{
  val in = Input(UInt(2.W))
  val out = Output(new SObj)
}

class Trial extends Module {
  val io = IO(new TrialIO)

  val sObj = Wire(new SObj)

  sObj.a := io.in
  sObj.b := ~io.in

  io.out := sObj

}
