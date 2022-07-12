package armflex

import chisel3._
import chisel3.util._
import arm.PROCESSOR_TYPES._

class FetchAndTransCtrlCommit(thidN: Int) extends Bundle {
  val fetchNext = Valid(new Bundle {
    val thid = UInt(log2Ceil(thidN).W)
    val nextPC = DATA_T
    val isUndef = Bool()
    val isException = Bool()
  })
}

class FetchAndTransCtrlTransplant(thidN: Int) extends Bundle {
  val fetchNext = Valid(new Bundle {
    val thid = UInt(log2Ceil(thidN).W)
    val nextPC = DATA_T
    val isStartNormal = Bool()
    val isStartSingleStep = Bool()
  })
  val stopNextCommit = Valid(new Bundle {
    val thidMask = UInt(thidN.W)
  })
  val transplant = Valid(new Bundle {
    val thid = UInt(log2Ceil(thidN).W)
  })
}

class FetchAndTransCtrlArchState(thidN: Int) extends Bundle {
  val getArchStateStatus = new Bundle {
    val thid = Output(UInt(log2Ceil(thidN).W))
    val isLastCommit = Input(Bool())
  }
}

class FetchAndTransCtrlFetch(thidN: Int) extends Bundle {
  val fetchNext = Vec(4, Valid(new Bundle {
    val thid = UInt(log2Ceil(thidN).W)
    val nextPC = Valid(DATA_T)
  }))
}

class FetchAndTransCtrlMMU(thidN: Int) extends Bundle {
  val wakeupInst = Valid(new Bundle {
    val thid = UInt(log2Ceil(thidN).W)
  })
  val wakeupData = Valid(new Bundle {
    val thid = UInt(log2Ceil(thidN).W)
  })
}

class TransFetchCtrl(thidN: Int) extends Module {
  val io = IO(new Bundle {
    val commitU = new FetchAndTransCtrlCommit(thidN)
    val transplantU = new FetchAndTransCtrlTransplant(thidN)
    val archstateU = new FetchAndTransCtrlArchState(thidN)
    val mmu = new FetchAndTransCtrlMMU(thidN)
    val fetchU = new FetchAndTransCtrlFetch(thidN)
  })  


  // Transplant
  io.fetchU.fetchNext(0).bits.thid := io.transplantU.fetchNext.bits.thid
  io.fetchU.fetchNext(0).bits.nextPC.bits := io.transplantU.fetchNext.bits.nextPC
  io.fetchU.fetchNext(0).bits.nextPC.valid := true.B
  // FetchNext after commit
  io.fetchU.fetchNext(1)
  // Wakeup after mmu page fault
  io.fetchU.fetchNext(2)
  io.fetchU.fetchNext(3)

}
