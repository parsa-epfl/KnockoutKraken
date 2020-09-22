package armflex.util

import chiseltest._
import chiseltest.internal._

import armflex._

object SimTools {
  def log(str: String)(implicit cfgProc: ProcConfig) = if(cfgProc.simVerbose) {
    val withClock = true
    if(withClock) {
      val cycle = Context().backend.getClockCycle()
      println(s"${"%3d".format((cycle-1)*2)}ns:" + str)
    } else {
      println(str)
    }
  }
}


