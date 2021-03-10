package armflex.demander.peripheral

import chisel3._
import chisel3.util._

import armflex.demander.software_bundle

import armflex.cache.{
  CacheFrontendFlushRequest,
  CacheParameter,
}

/**
 * The buffer where the interactive pages are stored.
 * It contains a normal R/W port(For page deleater and page inserter) and a AXI slave port(for QEMU)
 */ 
class PageBuffer extends MultiIOModule {
  import antmicro.Bus._
  import armflex.util._

  class page_buffer_write_request_t extends Bundle {
    val addr = UInt(10.W) // TODO: Make external and let it becomes a parameter.
    val data = UInt(512.W) // TODO: Make external.
    val last_v = Bool()
  }
  val normal_write_request_i = IO(Flipped(Decoupled(new page_buffer_write_request_t)))
  val normal_read_request_i = IO(Flipped(Decoupled(UInt(10.W))))
  val normal_read_reply_o = IO(Decoupled(UInt(512.W)))

  // S_AXI base address: 
  val S_AXI = IO(Flipped(new AXI4(16, 512))) // TODO: Restrict the address rage of this AXI bus.

  val u_converter = Module(new AXIRAMController(16, 512))
  u_converter.S_AXI <> S_AXI

  // BRAM Internal Address| Meaning| AXI Page number
  // 0-63|Page to QEMU|0
  // 64-127|Page from QEMU|1

  val bramCfg = new BRAMConfig(
    512 / 8,
    8,
    1024
  )

  val u_bram = Module(new BRAM()(bramCfg))
  val page_to_qemu_vr = RegInit(false.B)
  val page_from_qemu_vr = RegInit(false.B)

  // Port A is for read request
  val u_port_a_arb = Module(new RRArbiter(UInt(10.W), 2))

  u_port_a_arb.io.in(0).bits := normal_read_request_i.bits
  u_port_a_arb.io.in(0).valid := normal_read_request_i.valid && page_from_qemu_vr
  normal_read_request_i.ready := u_port_a_arb.io.in(0).ready && page_from_qemu_vr

  u_port_a_arb.io.in(1).bits := u_converter.read_request_o.bits(15, 6)
  u_port_a_arb.io.in(1).valid := u_converter.read_request_o.valid && page_to_qemu_vr && u_converter.read_request_o.bits(15, 12) === 1.U
  u_converter.read_request_o.ready := u_port_a_arb.io.in(1).ready && page_to_qemu_vr

  val chosen_port = Wire(Decoupled(u_port_a_arb.io.chosen.cloneType))
  chosen_port.valid := u_port_a_arb.io.out.valid
  u_port_a_arb.io.out.ready := chosen_port.ready
  chosen_port.bits := u_port_a_arb.io.chosen

  val chosen_port_q = Queue(chosen_port, 1, true)

  u_bram.portA.ADDR := u_port_a_arb.io.out.bits
  u_bram.portA.DI := DontCare
  u_bram.portA.EN := chosen_port.fire()
  u_bram.portA.WE := 0.U

  chosen_port_q.ready := Mux(chosen_port_q.bits === 0.U, normal_read_reply_o.ready, u_converter.read_reply_i.ready)

  normal_read_reply_o.bits := u_bram.portA.DO
  normal_read_reply_o.valid := chosen_port_q.valid && chosen_port_q.bits === 0.U

  u_converter.read_reply_i.bits := u_bram.portA.DO
  u_converter.read_reply_i.valid := chosen_port_q.valid && chosen_port_q.bits === 1.U

  // Port B is for write request
  val u_port_b_arb = Module(new RRArbiter(u_converter.write_request_o.bits.cloneType, 2))

  u_port_b_arb.io.in(0).bits.addr := normal_write_request_i.bits.addr
  u_port_b_arb.io.in(0).bits.data := normal_write_request_i.bits.data
  u_port_b_arb.io.in(0).bits.mask := Fill(512 / 8, 1.U(1.W))
  u_port_b_arb.io.in(0).valid := normal_write_request_i.valid && !page_to_qemu_vr
  normal_write_request_i.ready := u_port_b_arb.io.in(0).ready && !page_to_qemu_vr

  u_port_b_arb.io.in(1).bits.addr := u_converter.write_request_o.bits.addr(15, 6)
  u_port_b_arb.io.in(1).bits.data := u_converter.write_request_o.bits.data
  u_port_b_arb.io.in(1).bits.mask := u_converter.write_request_o.bits.mask
  u_port_b_arb.io.in(1).valid := u_converter.write_request_o.valid && !page_from_qemu_vr && (u_converter.write_request_o.bits.addr(15, 12) === 1.U)

  u_converter.write_request_o.ready := u_port_b_arb.io.in(1).ready && !page_from_qemu_vr

  u_bram.portB.ADDR := u_port_b_arb.io.out.bits.addr
  u_bram.portB.DI := u_port_b_arb.io.out.bits.data
  u_bram.portB.EN := u_port_b_arb.io.out.valid
  u_bram.portB.WE := u_port_b_arb.io.out.bits.mask
  u_port_b_arb.io.out.ready := true.B

  // update logic of page_to_qemu_vr
  when(normal_write_request_i.fire() && normal_write_request_i.bits.last_v){
    page_to_qemu_vr := true.B
  }.elsewhen(u_converter.read_request_o.fire() && u_converter.read_request_o.bits === 0x0FC0.U){
    // The last entry is read by QEMU
    page_to_qemu_vr := false.B
  }

  // update logic of page_from_qemu_vr
  when(normal_read_request_i.bits === 127.U && normal_read_request_i.fire()){
    page_from_qemu_vr := false.B
  }.elsewhen(u_converter.write_request_o.fire() && u_converter.write_request_o.bits.addr === 0x1FC0.U){
    // the last entry is put by QEMU
    page_from_qemu_vr := true.B
  }
}

object PageBufferVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage
  println(c.emitVerilog(new PageBuffer))
}
