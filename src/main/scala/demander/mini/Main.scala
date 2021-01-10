// See LICENSE for license details.

package armflex.demander.mini

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import java.io.File

class MiniRVSystem (
  i_context: String = ""
)(
  implicit val p: MiniConfig
) extends MultiIOModule {
  val u_core = Module(new Core())

  val u_ibuf = Mem(16 * 1024, UInt(32.W)) // 14
  if(!i_context.isEmpty() && new File(i_context).exists()){
    loadMemoryFromFile(u_ibuf, i_context)
  }

  val u_dbuf = Mem(16 * 1024, Vec(4, UInt(8.W))) // 14

  // Very straightforward to bind the I Cache?
  val i_addr = u_core.io.icache.req.bits.addr(15, 2)
  u_core.io.icache.resp.bits.data := u_ibuf(i_addr)
  u_core.io.icache.resp.valid := true.B
  
  val d_addr = u_core.io.dcache.req.bits.addr
  val originValue = u_dbuf(d_addr(15, 2))
  u_core.io.dcache.resp.bits.data := Mux(
    d_addr(31, 16) === 1.U,
    originValue.asUInt(),
    0.U
  )
  u_core.io.dcache.resp.valid := true.B
  when(u_core.io.dcache.req.valid && !u_core.io.dcache.abort && u_core.io.dcache.req.bits.addr(31, 16) === 1.U){
    //u_core.io.dcache.req.bits.data,
    u_dbuf.write(
      d_addr(15, 2),
      VecInit(u_core.io.dcache.req.bits.data.asBools().grouped(8).map(VecInit(_).asUInt()).toSeq),
      u_core.io.dcache.req.bits.mask.asBools()
    )
  }

  u_core.io.host.fromhost.valid := false.B
  u_core.io.host.fromhost.bits := DontCare
}

object Main extends App {
  import chisel3.stage.ChiselStage
  println((new ChiselStage).emitVerilog(new MiniRVSystem("abcdef")(new MiniConfig)))
}
