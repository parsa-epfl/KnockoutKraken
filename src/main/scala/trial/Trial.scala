package trial

import chisel3._

class TrialVecs extends Module {

  val io = IO(new Bundle {
                val out = Output(UInt(8.W))

                val out0 = Output(UInt(8.W))
                val out1 = Output(UInt(8.W))
                val out2 = Output(UInt(8.W))
                val out3 = Output(UInt(8.W))
              })

  val reg = RegInit(0.U(2.W))
  val vec = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
  vec.zipWithIndex map { case (r,i) => r := r + i.asUInt}

  reg := (reg + 1.U)

  io.out := vec(reg)

  io.out0 := vec(0.U)
  io.out1 := vec(1.U)
  io.out2 := vec(2.U)
  io.out3 := vec(3.U)
}

class TrialRotate extends Module {

  val io = IO(new Bundle {
    val in  = Input(UInt(4.W))
    val rot = Input(UInt(2.W))
    val out = Output(UInt(4.W))
  })

  def rotateRight[T <: Data](norm: Vec[T], rot: UInt): Vec[T] = {
    val n = norm.size
    VecInit.tabulate(n) { i =>
      Mux(rot < (n - i).U, norm(rot - (n - i).U), norm(i.U + rot))
    }
  }

  val vec = VecInit(io.in.toBools)
  io.out := rotateRight(vec, io.rot).asUInt
}
