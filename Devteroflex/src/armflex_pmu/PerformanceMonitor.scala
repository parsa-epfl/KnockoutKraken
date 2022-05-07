package armflex_pmu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import antmicro.CSR.CSR


class PerformanceMonitor(threadNumber: Int = 128) extends Module {
  // 6 counters:
  //  1. Time to resolve a single data cache miss.
  //  2. Time to resolve a data TLB miss
  //  3. Time to transplant
  //  4. Time to resolve a page fault.

  //  5. Number of instructions committed.
  //  6. Cycles between start and stop. (Controlled by the software)

  private val counterBits = 16
  private val counterNumber = 16

  val uDCachePenaltyCnt = Module(new CycleCounters(counterNumber, counterBits, threadNumber))
  val uTLBPenaltyCnt = Module(new CycleCounters(counterNumber, counterBits, threadNumber))
  val uTransplantPenaltyCnt = Module(new CycleCounters(counterNumber, counterBits, threadNumber))
  val uPageFaultPenaltyCnt = Module(new CycleCounters(counterNumber, counterBits, threadNumber))

  val rInstructionCommitted = RegInit(0.U(64.W))

  val iCycleCountingReq = IO(Input(Vec(4, new CycleCountingPort(threadNumber))))
  uDCachePenaltyCnt.iReq <> iCycleCountingReq(0)
  uTLBPenaltyCnt.iReq <> iCycleCountingReq(1)
  uTransplantPenaltyCnt.iReq <> iCycleCountingReq(2)
  uPageFaultPenaltyCnt.iReq <> iCycleCountingReq(3)
  
  val iCommittedValid = IO(Input(Bool()))
  when(iCommittedValid){
    rInstructionCommitted := rInstructionCommitted + 1.U
  }

  val rCycleCounter = RegInit(0.U(64.W))
  val rCycleCounterRunning = RegInit(false.B)

  when(rCycleCounterRunning){
    rCycleCounter := rCycleCounter + 1.U
  }

  private val registerNumberPerCycleCounter = (counterBits * counterNumber) / 32
  val uCSR = Module(new CSR(32, 8 + registerNumberPerCycleCounter * 4))
  val S_CSR = IO(Flipped(uCSR.io.bus.cloneType))
  S_CSR <> uCSR.io.bus

  // The CSR port:
  // - CSR[1]:CSR[0]: The number of cycles passed
  uCSR.io.csr(0).dataIn := rCycleCounter(31, 0)
  uCSR.io.csr(1).dataIn := rCycleCounter(63, 32)
    
  // - CSR[2]: Whether to start or stop the cycles.
  uCSR.io.csr(2).dataIn := rCycleCounterRunning
  when(uCSR.io.csr(2).dataWrite){
    rCycleCounterRunning := uCSR.io.csr(2).dataOut
  }
  // - CSR[4]:CSR[3]: The number of instructions committed.
  uCSR.io.csr(3).dataIn := rInstructionCommitted(31, 0)
  uCSR.io.csr(4).dataIn := rInstructionCommitted(63, 32)

  // 5 to 8 are empty.
  for(i <- 5 until 8){
    uCSR.io.csr(i).dataIn := 0.U
  }

  private def connectCountersToCSR(data: Vec[UInt], startIndex: Int) = {
    assert(data.length == counterNumber)
    val otherFormat = data.asTypeOf(Vec(registerNumberPerCycleCounter, UInt(32.W)))
    for(i <- 0 until registerNumberPerCycleCounter){
      uCSR.io.csr(startIndex + i).dataIn := otherFormat(i)
    }
  }

  // - CSR[08] ~ CSR[15]: counters of uDCachePenaltyCnt
  connectCountersToCSR(uDCachePenaltyCnt.oCounters, 8)
  // - CSR[16] ~ CSR[23]: counters of uTLBPenaltyCnt
  connectCountersToCSR(uTLBPenaltyCnt.oCounters, 8 + registerNumberPerCycleCounter)
  // - CSR[24] ~ CSR[31]: counters of uTransplantPenaltyCnt
  connectCountersToCSR(uTransplantPenaltyCnt.oCounters, 8 + registerNumberPerCycleCounter * 2)
  // - CSR[32] ~ CSR[39]: counters of uPageFaultPenaltyCnt
  connectCountersToCSR(uPageFaultPenaltyCnt.oCounters, 8 + registerNumberPerCycleCounter * 3)
}
