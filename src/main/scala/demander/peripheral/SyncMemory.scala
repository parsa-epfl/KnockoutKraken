package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import java.io.File
import chisel3.util.experimental.loadMemoryFromFile

class SyncMemory(
  entryNumber: Int,
)(
  initialFile: String = ""
) extends MultiIOModule {
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(32, 32))))
  val reply_o = IO(Output(UInt(32.W)))

  val internalAddressWidth = log2Ceil(entryNumber)
  require(internalAddressWidth == 14)
  val internal_address = request_i.bits.addr(internalAddressWidth + 1, 2)

  val u_bank = Mem(entryNumber, UInt(32.W))
  reply_o := RegNext(u_bank.read(internal_address)) //(internal_address)
  when(request_i.valid && request_i.bits.w_v){
    // write
    val full_mask = request_i.bits.w_mask.asBools().map(Fill(8, _).asBools()).flatten
    val data_to_write = full_mask.zip(request_i.bits.data.asBools().zip(reply_o.asBools())).map { 
      case (sel: Bool, (w: Bool, r: Bool)) => Mux(sel, w, r)
    }
    u_bank(internal_address) := VecInit(data_to_write).asUInt()
  }

  if(initialFile.nonEmpty && new File(initialFile).exists()){
    loadMemoryFromFile(u_bank, initialFile)
  }
}
