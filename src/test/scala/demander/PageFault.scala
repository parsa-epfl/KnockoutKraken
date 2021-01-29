package armflex.demander

import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.experimental._

import TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation
import firrtl.options.TargetDirAnnotation


import org.scalatest.FreeSpec
import armflex.cache.MemorySystemParameter


class PageDemanderPageFaultTester extends FreeSpec with ChiselScalatestTester {
  "Normal" in {
    val anno = Seq(TargetDirAnnotation("test/demander/pagefault/normal"), VerilatorBackendAnnotation, WriteVcdAnnotation)
    test(new PageDemander(new MemorySystemParameter(), "")).withAnnotations(anno){ dut =>

    }
  }
}