package protoflex

import chisel3.{core, _}
import chisel3.util._
import common.{BRAMConfig, BRAMPort}
import common.PROCESSOR_TYPES._

/*
* Transplant unit: do following function
* 1. Initialize Proc : read processor state from Bram and update Proc
* 2. Transplant : flush pipeline and write back state to Bram
* */
class TransplantUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  implicit val stateBRAMc = cfg.stateBRAMc
  // TP <-> proc

  val flush = Output(Bool())
  val tp_en = Output(Bool())
  val tp_req = Input(Bool())
  val tp_tag = Output(TAG_T)
  val fetch_start = Output(Bool())
  //general purpose register
  val tp_reg_waddr = Output(REG_T)
  val tp_reg_wdata = Output(DATA_T)
  val tp_reg_wen = Output(Bool())

  val tp_reg_raddr = Output(REG_T) // there are two read port but only use one
  val tp_reg_rdata = Input(DATA_T)
  // pstate
  val tp_pstate_wen = Output(Bool())
  val tp_pstate_in = Input(new PStateRegs)
  val tp_pstate_out = Output(new PStateRegs)


  // TP <--> magic block
  val start = Input(Bool())
  val done = Output(Bool())

  // TP <--> Bram for now use bram interface
  val bram_port = Flipped(new BRAMPort(1))

  // decouble bram interface from module
  //  val raddr = Output(DATA_T)
  //  val rdata = Input(DATA_T)
  //  val rd_en = Output(Bool())
  //  val wdata = Input(INST_T)
  //  val waddr = Output(DATA_T)
  //  val w_en = Output(Bool())
}


class TransplantUnit(implicit val cfg: ProcConfig) extends Module{
  val io = IO(new TransplantUnitIO)

  val g_reg_count = RegInit(0.U(REG_W))
  def addr_width = cfg.stateBRAMc.addrWidthVec(1)
  def GEN_ADDR = 0.U
  def SP_ADDR = 64.U
  val bram_base = RegInit(0.U(addr_width.W))
  val bram_offset = Wire(UInt(addr_width.W))
  val sp_reg_count = RegInit(0.U(SP_REG_N.W))
  // for now use wait_r to read 64 bit data (2cycles)
  val wait_r = RegInit(0.U(1.W))
  val pc_wait = RegInit(0.U(1.W))
  val sp_cnt = RegInit(0.U(8.W))
  def resetCounter(): Unit ={
    g_reg_count := 0.U
    sp_reg_count := 0.U
    sp_cnt := 0.U
    wait_r := 0.U
    pc_wait := 0.U
    done_r := 0.U
  }
  val w_pstate_reg = RegInit(Wire(new PStateRegs()).empty)
  val bram_out_r = RegNext(io.bram_port.dataOut.get)
  val done_r = RegInit(0.U(1.W))


  // default values
  io.tp_tag := 0.U // TODO: Transplant multiple threads
  val wire_64bits = WireInit(Cat(bram_out_r(31,0), io.bram_port.dataOut.get(31,0)))
  val tp_pstate_wen = WireInit(false.B)
  io.tp_pstate_out := w_pstate_reg
  val w_pstate_pc_en = WireInit(false.B)
  val w_pstate_sp_en = WireInit(false.B)
  val w_pstate_el_en = WireInit(false.B)
  val w_pstate_nzcv_en = WireInit(false.B)

  val tp_reg_wen = WireInit(false.B)
  io.flush := io.tp_req
  io.done := done_r
  io.fetch_start := false.B
  bram_offset := 0.U

  // bram interface
  io.bram_port.addr := bram_base + bram_offset
  io.bram_port.dataIn.get := 0.U
  io.bram_port.writeEn.get := 0.U

  val s_IDLE :: bram_to_proc_reg:: bram_to_proc_pstate :: proc_to_bram_reg  :: proc_to_bram_pstate :: Nil = Enum(5)
  val state = RegInit(s_IDLE)
  io.tp_en := (state =/= s_IDLE)
  io.bram_port.en := (state =/= s_IDLE)

  switch(state) {
    is(s_IDLE) {
      when(io.start) {
        state := bram_to_proc_reg
        bram_base := GEN_ADDR
        resetCounter()
      }
      when(io.tp_req){
        state := proc_to_bram_reg
        bram_base := GEN_ADDR
        resetCounter()
      }
    }
    is(proc_to_bram_reg){
      bram_offset := (g_reg_count<<1.U) | wait_r
      io.bram_port.writeEn.get := true.B
      wait_r := wait_r + 1.U
      when( wait_r === 0.U){
        io.bram_port.dataIn.get := io.tp_reg_rdata(DATA_SZ - 1,DATA_SZ/2)
      }.otherwise{
        io.bram_port.dataIn.get := io.tp_reg_rdata((DATA_SZ/2)-1,0)
        g_reg_count := g_reg_count + 1.U
        when(g_reg_count === (REG_N - 1).U){
          state := proc_to_bram_pstate
          bram_base := SP_ADDR
        }
      }
    }
    is(proc_to_bram_pstate){
      sp_cnt := sp_cnt + 1.U
      bram_offset :=  sp_cnt
      io.bram_port.writeEn.get := true.B
      // do this in better way
      when(sp_reg_count === 0.U){
        pc_wait := pc_wait + 1.U
        when(pc_wait === 0.U){
          io.bram_port.dataIn.get := io.tp_pstate_in.PC(DATA_SZ - 1,DATA_SZ/2)
        }.otherwise{
          io.bram_port.dataIn.get := io.tp_pstate_in.PC((DATA_SZ/2)-1,0)
          sp_reg_count := sp_reg_count + 1.U
        }
      }.otherwise{
        sp_reg_count := sp_reg_count + 1.U
        when(sp_reg_count === 1.U){
          io.bram_port.dataIn.get := io.tp_pstate_in.SP
        }
        when(sp_reg_count === 2.U){
          io.bram_port.dataIn.get := io.tp_pstate_in.EL
        }
        when(sp_reg_count === 3.U){
          io.bram_port.dataIn.get := io.tp_pstate_in.NZCV
        }
        when(sp_reg_count === SP_REG_N.U){
          done_r := true.B
          state := s_IDLE
        }
      }
    }
    is(bram_to_proc_reg){
      bram_offset := (g_reg_count<<1.U) | wait_r
      wait_r := wait_r + 1.U
      when(wait_r === 0.U){ // 1st clock cycle

      }.otherwise{ // 2nd clock cycle
        g_reg_count := g_reg_count + 1.U
        tp_reg_wen := true.B
        when(g_reg_count === (REG_N - 1).U){ // done
          state := bram_to_proc_pstate
          bram_base := SP_ADDR
        }
      }
    }
    is(bram_to_proc_pstate){ // TODO: multiple thread
      // ugly way, do it better, 1 cycle for each special register (32bit value from qemu)
      require(SP_REG_N == 4)

      sp_cnt := sp_cnt + 1.U
      bram_offset :=  sp_cnt
      when(sp_reg_count === 0.U){ // PC
        pc_wait := pc_wait + 1.U
        when(pc_wait === 1.U){
          w_pstate_pc_en := true.B
          sp_reg_count := sp_reg_count + 1.U
        }
      }.otherwise{
        sp_reg_count := sp_reg_count + 1.U
        when(sp_reg_count === 1.U){ // SP
          w_pstate_sp_en := true.B
        }
        when(sp_reg_count === 2.U){ // EL
          w_pstate_el_en := true.B
        }
        when(sp_reg_count === 3.U){ // NZCV
          w_pstate_nzcv_en := true.B
        }
        when(sp_reg_count === 4.U){ // Write back state
          tp_pstate_wen := true.B
        }
        when(sp_reg_count === 5.U){ // Done transplanting
          io.fetch_start := true.B
          state := s_IDLE
        }
      }
    }
  }
  io.tp_reg_wen := RegNext(tp_reg_wen)
  io.tp_reg_waddr := RegNext(g_reg_count)
  io.tp_reg_raddr := RegNext(g_reg_count)
  io.tp_reg_wdata := wire_64bits
  w_pstate_reg.PC := Mux(RegNext(w_pstate_pc_en), wire_64bits, w_pstate_reg.PC)
  w_pstate_reg.SP := Mux(RegNext(w_pstate_sp_en), io.bram_port.dataOut.get(31,0), w_pstate_reg.SP)
  w_pstate_reg.EL := Mux(RegNext(w_pstate_el_en), io.bram_port.dataOut.get(31,0), w_pstate_reg.EL)
  w_pstate_reg.NZCV := Mux(RegNext(w_pstate_nzcv_en), io.bram_port.dataOut.get(31,0), w_pstate_reg.NZCV)
  io.tp_pstate_wen := RegNext(tp_pstate_wen)
}
