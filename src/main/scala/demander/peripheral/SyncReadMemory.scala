package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import java.io.File
import chisel3.util.experimental.loadMemoryFromFile

class SyncReadMemory(
  entryNumber: Int,
  addressWidth: Int = 32,
  dataWidth: Int = 32
)(
  initialFile: String = ""
) extends MultiIOModule {
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(addressWidth, dataWidth))))
  val reply_o = IO(Output(UInt(dataWidth.W)))

  val internalAddressWidth = log2Ceil(entryNumber)
  val internal_address = request_i.bits.addr(internalAddressWidth - 1 + log2Ceil(dataWidth / 8), log2Ceil(dataWidth / 8))

  val u_bank = SyncReadMem(entryNumber, UInt(dataWidth.W))
  reply_o := u_bank(internal_address)

  when(request_i.valid && request_i.bits.w_v){
    // write
    val full_mask = request_i.bits.w_mask.asBools().map(Fill(8, _).asBools()).flatten
    val data_to_write = full_mask.zip(request_i.bits.data.asBools().zip(reply_o.asBools())).map { 
      case (sel: Bool, (w: Bool, r: Bool)) => Mux(sel, w, r)
    }
    u_bank(internal_address) := VecInit(data_to_write).asUInt()
  }

  if(new File(initialFile).exists()){
    loadMemoryFromFile(u_bank, initialFile)
  }
}
