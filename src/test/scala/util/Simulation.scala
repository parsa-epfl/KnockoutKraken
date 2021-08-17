package armflex.util

import chiseltest._
import chiseltest.internal._

import armflex._

object SimTools {
  def log(str: String)(implicit cfgProc: PipelineParams) = if(cfgProc.simVerbose) {
    val withClock = true
    if(withClock) {
      val cycle = -1 // Context().backend.getClockCycle() // Need to compile modified chisel-testers2
      println(s"${"%3d".format((cycle-1)*2)}ns:" + str)
    } else {
      println(str)
    }
  }
}


