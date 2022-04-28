package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chiseltest.internal._

import firrtl.options.TargetDirAnnotation

import chiseltest.simulator.WriteVcdAnnotation
import antmicro.util.CSRDrivers._
import armflex.util.AXIDrivers.AXI4Driver
import util.{Queue}
import antmicro.Bus.AXI4

import AXIControlledMessageQueueDrivers._

class AXIControlledMessageQueueTest extends AnyFreeSpec with ChiselScalatestTester {
  val data = Seq(0x1000.U, 0x2000.U, 0x3000.U)
  "Setup fifo entries" in {
    test(new AXIControlledMessageQueueWrapper(512, 32, 4)).withAnnotations(Seq(
    VerilatorBackendAnnotation, TargetDirAnnotation("test/AXIFIFOQueue/Fifos"))) {
      dut => 
        dut.init()
        dut.pushRead(data)
        dut.pushWriteExpect(data)
        dut.expectMsgCount(data.size, data.size)
        dut.clock.step()
    }
  }
  
  "Try read fifo" in {
    test(new AXIControlledMessageQueueWrapper(512, 32, 4)).withAnnotations(Seq(
    VerilatorBackendAnnotation, TargetDirAnnotation("test/AXIFIFOQueue/Rd"))) {
      dut => 
        dut.init()
        dut.pushRead(data)
        dut.sendRead(data)
        dut.clock.step()
    }
  }

  "Try write fifo" in {
    test(new AXIControlledMessageQueueWrapper(512, 32, 4)).withAnnotations(Seq(
    VerilatorBackendAnnotation, TargetDirAnnotation("test/AXIFIFOQueue/Wr"), WriteVcdAnnotation)) {
      dut => 
        dut.init()
        dut.pushWriteExpect(data)
        dut.sendWrite(data)
        dut.clock.step()
    }
  }

  "Try write fail" in {
    test(new AXIControlledMessageQueueWrapper(512, 32, 4)).withAnnotations(Seq(
    VerilatorBackendAnnotation, TargetDirAnnotation("test/AXIFIFOQueue/WrFail"))) {
      dut => 
        dut.init()
        dut.sendWrite(data, AXI4.SLVERR)
        dut.clock.step()
    }
  }

  "Try read fail" in {
    test(new AXIControlledMessageQueueWrapper(512, 32, 4)).withAnnotations(Seq(
    VerilatorBackendAnnotation, TargetDirAnnotation("test/AXIFIFOQueue/RdFail"))) {
      dut => 
        dut.init()
        dut.sendRead(data, AXI4.SLVERR)
        dut.clock.step()
    }
  }

  "Try read available write and read entries" in {
    test(new AXIControlledMessageQueueWrapper(512, 32, 4)).withAnnotations(Seq(
    VerilatorBackendAnnotation, TargetDirAnnotation("test/AXIFIFOQueue/queueCnts"), WriteVcdAnnotation)) {
      dut => 
        dut.init()
        dut.rdFifo.msgCnt.poke(4.U)
        dut.wrFifo.freeCnt.poke(3.U)
        dut.clock.step()
        dut.expectMsgCount(4, 3)
    }
  }
  
}

object AXIControlledMessageQueueDrivers {
  class AXIControlledMessageQueueWrapper(val axiW: Int, val csrRegSize: Int, val queueS: Int) extends Module {
    val dut = Module(new AXIControlledMessageQueue(axiW, csrRegSize, queueS))
    val rdFifo = IO(dut.rdFifo.cloneType)
    val wrFifo = IO(dut.wrFifo.cloneType)
    val S_AXI  = IO(Flipped(dut.S_AXI.cloneType))
    val S_CSR  = IO(Flipped(dut.S_CSR.cloneType))
    dut.S_AXI <> S_AXI
    dut.S_CSR <> S_CSR
    dut.rdFifo <> rdFifo
    dut.wrFifo <> wrFifo

    val wrFifoExpect = IO(Flipped(dut.wrFifo.enq.cloneType))
    val rdFifoQ = Module(new Queue(dut.rdFifo.deq.bits.cloneType, 512, true, true))
    val wrFifoQ = Module(new Queue(dut.wrFifo.enq.bits.cloneType, 512, true, true))
    rdFifoQ.io.enq <> rdFifo.deq
    wrFifoQ.io.enq <> wrFifoExpect 
    dut.rdFifo.deq <> rdFifoQ.io.deq

    dut.wrFifo.enq.ready := true.B
    wrFifoQ.io.deq.ready := dut.wrFifo.enq.fire
    when(dut.wrFifo.enq.fire && wrFifoQ.io.deq.valid) {
      assert(dut.wrFifo.enq.bits === wrFifoQ.io.deq.bits)
    }
  }

  implicit class AXIControlledMessageQueueDriver(target: AXIControlledMessageQueueWrapper) {
    implicit val clock: Clock = target.clock
    def init() = {
      target.rdFifo.deq.setSourceClock(clock)
      target.wrFifo.enq.setSinkClock(clock)
      target.rdFifo.deq.initSource()
      target.wrFifo.enq.initSink()
      target.wrFifoExpect.setSourceClock(clock)
      target.wrFifoExpect.initSource()
      target.rdFifo.msgCnt.poke(0.U)
      target.wrFifo.freeCnt.poke(0.U)
      target.S_AXI.initSlave()
      target.S_CSR.init()
    }
    
    def pushRead(data: Seq[UInt]) = {
      data.foreach(target.rdFifo.deq.enqueue(_))
      target.rdFifo.msgCnt.poke(data.size.U)
    }
    
    def pushWriteExpect(data: Seq[UInt]) = {
      data.foreach(target.wrFifoExpect.enqueue(_))
      target.wrFifo.freeCnt.poke(data.size)
    }
    
    def expectMsgCount(msgCnt: Int, freeCnt: Int) = {
      assert(target.S_CSR.readReg(0) == msgCnt)
      assert(target.S_CSR.readReg(1) == freeCnt)
    }
    
    def sendWrite(data: Seq[UInt]) = target.S_AXI.wr((target.axiW/8).U, data)
    def sendWrite(data: Seq[UInt], expectedFlag: Int) = target.S_AXI.wr((target.axiW/8).U, data, expectedFlag)
    def sendRead(dataExpect: Seq[UInt]) = target.S_AXI.rd(0x0.U, dataExpect)
    def sendRead(dataExpect: Seq[UInt], expectedFlag: Int) = target.S_AXI.rd(0x0.U, dataExpect, expectedFlag)
  }
}