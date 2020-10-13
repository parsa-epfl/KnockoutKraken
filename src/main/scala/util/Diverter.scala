package armflex.util

import chisel3._
import chisel3.util._

/**
 *  A dataflow diverter with decoupled port.
 *  
 *  Use this module to connect multiple ports to one without considering duplicated pushing problem. 
 */ 
class Diverter[T <: Data](wayNumber: Int, t: T) extends Module{
  val io = IO(new Bundle{
    val i = Flipped(Decoupled(t)) // keep v until ack is given.
    val ack_o = Output(Bool()) // v -> ack: ack is determined by valid
    val o = Vec(wayNumber, Decoupled(t)) // v & r
  })

  val pushed_b = WireInit(VecInit(Seq.fill(wayNumber)(false.B))) // whether this channel can be pushed.
  val pushed_r = RegInit(VecInit(Seq.fill(wayNumber)(false.B))) // whether this channel is pushed.
  for(i <- 0 until wayNumber){
    io.o(i).valid := Mux(pushed_r(i), false.B, io.i.valid)
    pushed_b(i) := io.o(i).valid && io.o(i).ready
    pushed_r(i) := Mux(io.ack_o, false.B, pushed_b(i))
  }

  io.ack_o := pushed_r.zip(pushed_b).map({y =>
    y._1 || y._2
  }).reduce({(x, y) =>
    x && y
  })

  io.i.ready := pushed_r.asUInt() === 0.U || io.ack_o
}

// grammar surger for initializing a Diverter.
object Diverter{
  /**
   * Generate a dataflow diverter.
   * 
   * @param wayNumber The number of output channels.
   * @param input The input channel.
   * @return The output channels
   * 
   * @example {{{
   *  val output = Diverter(2, input)
   *  p1 <> output(0)
   *  p2 <> output(1)
   * }}}
   */ 
  def apply[T <: Data](wayNumber: Int, input: DecoupledIO[T]): Vec[DecoupledIO[T]]  = {
    val res = Module(new Diverter(wayNumber, input.bits.cloneType))
    res.io.i <> input
    res.io.o
  }
}
