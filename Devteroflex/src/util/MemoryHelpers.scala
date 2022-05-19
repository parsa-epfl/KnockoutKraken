package armflex.util

import chisel3._
import chisel3.util._

import antmicro.Bus.AXI4
import armflex.util.ExtraUtils._
import dataclass.data
import antmicro.Frontend.AXI4Reader
import antmicro.Frontend.AXI4Writer
import antmicro.Bus._

class MemReqBurst(
    val addrW: Int,
    val dataW: Int,
    val burstS: Int = 8
) extends Bundle {
    val addr = UInt(addrW.W)
    val w_en = UInt((dataW/8).W)
    val burst = UInt(burstS.W)
}

class ReadPort(
    val addrW: Int,
    val dataW: Int,
    val burstS: Int = 8
) extends Bundle {
    val req = Flipped(Decoupled(new MemReqBurst(addrW, dataW, burstS)))
    val data = Decoupled(UInt(dataW.W))
}

class WritePort(
    val addrW: Int,
    val dataW: Int,
    val burstS: Int = 8
) extends Bundle {
    val req = Flipped(Decoupled(new MemReqBurst(addrW, dataW, burstS)))
    val data = Flipped(Decoupled(UInt(dataW.W)))
}

class DRAMPortParams(
  val pAddrW: Int = 12,
  val pDataW: Int = 512
)
class DRAMWrapperWrite(val params: DRAMPortParams) extends Module {
  assert(params.pDataW == 512, "Assumes that we match AWS F1 DRAM width.")
  val M_AXI_W = IO(new AXIWriteMasterIF(params.pAddrW, params.pDataW))
  // The write port allows for burst data writes
  val write = IO(new WritePort(params.pAddrW, params.pDataW))

  M_AXI_W.req.bits.address := write.req.bits.addr
  M_AXI_W.req.bits.length := write.req.bits.burst
  M_AXI_W.req.handshake(write.req)
  M_AXI_W.data <> write.data
}

class DRAMWrapperRead(val params: DRAMPortParams) extends Module {
  assert(params.pDataW == 512, "Assumes that we match AWS F1 DRAM width.")
  val M_AXI_R = IO(new AXIReadMasterIF(params.pAddrW, params.pDataW))

  // The read port allows for burst data writes
  val read = IO(new ReadPort(params.pAddrW, params.pDataW))
  M_AXI_R.req.bits.address := read.req.bits.addr
  M_AXI_R.req.bits.length := read.req.bits.burst
  M_AXI_R.req.handshake(read.req)
  read.data <> M_AXI_R.data
}

class AXI4MasterDMACtrl(val rdPortsN: Int, val wrPortsN: Int, 
                        val addrW: Int, val dataW: Int) extends Module {
  val req = IO(new Bundle {
    val wrPorts = Vec(wrPortsN, new ReadPort(addrW, dataW, 32))
    val rdPorts = Vec(rdPortsN, new WritePort(addrW, dataW, 32))
  })
  val M_AXI = IO(new AXI4(addrW, dataW))

  val sWaitReq :: sWrArbSetAddr :: sRdArbSetAddr :: sWrArbRunning :: sRdArbRunning :: Nil = Enum(5)
  val state_r = RegInit(sWaitReq)
  val isLastWrite = RegInit(false.B)

  val wrArbiterWorking_r = RegInit(false.B)
  val rdArbiterWorking_r = RegInit(false.B)

  val uWriter = Module(new AXI4Writer(addrW, dataW))
  val uReader = Module(new AXI4Reader(addrW, dataW))
  val uArbiterRd = Module(new RRArbiter(UInt(log2Ceil(rdPortsN).W), rdPortsN))
  val uArbiterWr = Module(new RRArbiter(UInt(log2Ceil(wrPortsN).W), wrPortsN))

  val biggestPort = Math.max(rdPortsN, wrPortsN).toInt
  val selectedPort = RegInit(0.U((log2Ceil(biggestPort) + 1).W))

  for (port <- 0 until rdPortsN) {
    // uArbiterRd.io.in(port).bits <> req.rdPorts(port)
    uArbiterRd.io.in(port).bits := port.U
    uArbiterRd.io.in(port).valid := req.rdPorts(port).req.valid
    req.rdPorts(port).req.ready := uArbiterRd.io.in(port).ready
    req.rdPorts(port).data.bits := uReader.io.dataOut.bits
    req.rdPorts(port).data.valid := false.B
  }

  for (port <- 0 until wrPortsN) {
    // uArbiterRd.io.in(port).bits <> req.rdPorts(port)
    uArbiterWr.io.in(port).bits := port.U
    uArbiterWr.io.in(port).valid := req.wrPorts(port).req.valid
    req.wrPorts(port).req.ready := uArbiterWr.io.in(port).ready
    req.wrPorts(port).data.ready := false.B
  }
   // Arbiter only manages 'selectedPort', popping the entries happens in the state machine
  uArbiterWr.io.out.ready := false.B
  uArbiterRd.io.out.ready := false.B // Arbiter only managed 'selectedPort'

  // Handle Address
  uReader.io.xfer.address := req.rdPorts(selectedPort).req.bits.addr
  uWriter.io.xfer.address := req.wrPorts(selectedPort).req.bits.addr
  uReader.io.xfer.length := req.rdPorts(selectedPort).req.bits.burst
  uWriter.io.xfer.length := req.wrPorts(selectedPort).req.bits.burst
  uReader.io.xfer.valid := state_r === sRdArbSetAddr
  uWriter.io.xfer.valid := state_r === sWrArbSetAddr
  req.rdPorts(selectedPort).req.ready := state_r === sRdArbSetAddr && uReader.io.xfer.ready
  req.wrPorts(selectedPort).req.ready := state_r === sWrArbSetAddr && uWriter.io.xfer.ready

  // Handle Data
  uReader.io.dataOut.bits <> req.rdPorts(selectedPort).data.bits
  uWriter.io.dataIn.bits <> req.wrPorts(selectedPort).data.bits
  when(state_r === sRdArbRunning) {
    uReader.io.dataOut.ready <> req.rdPorts(selectedPort).data.ready
    uReader.io.dataOut.valid <> req.rdPorts(selectedPort).data.valid
  }.otherwise {
    uReader.io.dataOut.ready := false.B
  }
  when(state_r === sWrArbRunning) {
    uWriter.io.dataIn.ready <> req.wrPorts(selectedPort).data.ready
    uWriter.io.dataIn.valid <> req.wrPorts(selectedPort).data.valid
  }.otherwise {
    uWriter.io.dataIn.valid := false.B
  }

  uWriter.io.bus.aw <> AXI4AW.stub(uWriter.io.bus.addrWidth)
  uWriter.io.bus.w <> AXI4W.stub(uWriter.io.bus.dataWidth)
  uWriter.io.bus.b <> AXI4B.stub()
  uWriter.io.bus.ar <> AXI4AR.stub(uWriter.io.bus.addrWidth)
  uWriter.io.bus.r <> AXI4R.stub(uWriter.io.bus.dataWidth)

  uReader.io.bus.aw <> AXI4AW.stub(uReader.io.bus.addrWidth)
  uReader.io.bus.w <> AXI4W.stub(uReader.io.bus.dataWidth)
  uReader.io.bus.b <> AXI4B.stub()
  uReader.io.bus.ar <> AXI4AR.stub(uReader.io.bus.addrWidth)
  uReader.io.bus.r <> AXI4R.stub(uReader.io.bus.dataWidth)

  when(wrArbiterWorking_r) {
    M_AXI <> uWriter.io.bus
  }.elsewhen(rdArbiterWorking_r) {
    M_AXI <> uReader.io.bus
  }.otherwise {
    M_AXI.aw <> AXI4AW.tieOff(uReader.io.bus.addrWidth)
    M_AXI.w  <> AXI4W.tieOff(uReader.io.bus.dataWidth)
    M_AXI.b  <> AXI4B.tieOff()
    M_AXI.ar <> AXI4AR.tieOff(uReader.io.bus.addrWidth)
    M_AXI.r  <> AXI4R.tieOff(uReader.io.bus.dataWidth)
  }

  switch(state_r) {
    is(sWaitReq) {
      wrArbiterWorking_r := false.B
      rdArbiterWorking_r := false.B
      when(uArbiterRd.io.out.valid && uArbiterWr.io.out.valid) {
        when(isLastWrite) {
          rdArbiterWorking_r := true.B
          selectedPort := uArbiterRd.io.out.bits
          state_r := sRdArbSetAddr
        }.otherwise {
          wrArbiterWorking_r := true.B
          selectedPort := uArbiterWr.io.out.bits
          state_r := sWrArbSetAddr
        }
      }.elsewhen(uArbiterRd.io.out.valid) {
        rdArbiterWorking_r := true.B
        selectedPort := uArbiterRd.io.out.bits
        state_r := sRdArbSetAddr
      }.elsewhen(uArbiterWr.io.out.valid) {
        wrArbiterWorking_r := true.B
        selectedPort := uArbiterWr.io.out.bits
        state_r := sWrArbSetAddr
      }
    }

    is(sRdArbSetAddr) {
      when(req.rdPorts(selectedPort).req.fire) {
        state_r := sRdArbRunning
      }
    }

    is(sWrArbSetAddr) {
      when(req.wrPorts(selectedPort).req.fire) {
        state_r := sWrArbRunning
      }
    }

    is(sRdArbRunning) {
      isLastWrite := false.B
      when(uReader.io.xfer.done) {
        state_r := sWaitReq
      }
    }

    is(sWrArbRunning) {
      isLastWrite := true.B
      when(uWriter.io.xfer.done) {
        state_r := sWaitReq
      }
    }
  }

  assert(!(wrArbiterWorking_r && rdArbiterWorking_r), "Can't have both workers hijack the AXI port simultaniously")
}
