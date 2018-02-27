package trial

import chisel3._

object TYPES
{
  def TYPE_A = UInt(8.W)
}


class SObj extends Bundle
{
  val a = TYPES.TYPE_A
  val b = TYPES.TYPE_A
}

class TrialIO extends Bundle
{
  val in = Input(UInt(8.W))
  val out = Output(new SObj)
}

class Trial extends Module {
  val io = IO(new TrialIO)

  val sObj = Wire(new SObj)

  sObj.a := io.in(7,4)
  sObj.b := ~io.in

  io.out := sObj

}
