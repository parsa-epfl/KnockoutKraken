package protoflex

import chisel3._
import chisel3.util._
import common.{BRAMConfig, BRAMPort}
import common.PROCESSOR_TYPES._


class TransplantUnitIO(implicit val cfg: ProcConfig) extends Bundle
{
  implicit val stateBRAMc = cfg.stateBRAMc
  // TP <-> proc
  val flush = Output(Bool())
  val tp_en = Output(Bool())
  val tp_req = Input(Bool())
  val tp_tag = Output(TAG_T)
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

  // TP <-> Bram for now use bram interface
  val start = Input(Bool())
  val done = Output(Bool())
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
  val sp_reg_count = RegInit(0.U(SP_REG_N.W))
  // for now use wait_r to read 64 bit data (2cycles)
  val wait_r = RegInit(0.U(1.W))
  def resetCounter(): Unit ={
    g_reg_count := 0.U
    sp_reg_count := 0.U
    wait_r := 0.U
  }
  val w_pstate_reg = RegInit(Wire(new PStateRegs()).empty)
  val bram_out_r = RegInit(io.bram_port.dataOut.get)


  // default values
  io.tp_tag := 0.U // TODO: Transplant multiple threads
  io.tp_reg_waddr := g_reg_count
  io.tp_reg_wdata := Cat(bram_out_r, io.bram_port.dataOut.get) // TODO: check endiness
  io.tp_pstate_wen := false.B
  io.tp_pstate_out := w_pstate_reg
  io.tp_reg_wen := false.B
  io.tp_reg_raddr := g_reg_count
  io.flush := io.tp_req

  //  io.flush := false.B

  // bram
  io.bram_port.addr := (g_reg_count << 1.U) | wait_r
  io.bram_port.dataIn.get := 0.U
  io.bram_port.writeEn.get := 0.U
  io.bram_port.en := false.B

  val s_IDLE :: write_gen_reg:: write_sp_reg :: read_gen_reg  :: read_sp_reg :: Nil = Enum(5)
  val state = RegInit(s_IDLE)
  io.tp_en := (state =/= s_IDLE)

  switch(state) {
    is(s_IDLE) {
      when(io.start) {
        state := write_gen_reg
        resetCounter()
      }
      when(io.tp_req){
        state := read_gen_reg
        resetCounter()
      }
    }
    is(read_gen_reg){
      io.bram_port.writeEn.get := true.B
      wait_r := wait_r + 1.U
      when( wait_r === 0.U){
        io.bram_port.dataIn.get := io.tp_reg_rdata(DATA_SZ - 1,DATA_SZ/2)
      }.otherwise{
        io.bram_port.dataIn.get := io.tp_reg_rdata((DATA_SZ/2)-1,0)
        g_reg_count := g_reg_count + 1.U
        when(g_reg_count === (REG_N - 1).U){
          state := read_sp_reg
        }
      }
    }
    is(read_sp_reg){
      sp_reg_count := sp_reg_count + 1.U
      // do this in better way
      when(sp_reg_count === 0.U){
        io.bram_port.dataIn.get := io.tp_pstate_in.PC
      }
      when(sp_reg_count === 1.U){
        io.bram_port.dataIn.get := io.tp_pstate_in.SP
      }
      when(sp_reg_count === 2.U){
        io.bram_port.dataIn.get := io.tp_pstate_in.EL
      }
      when(sp_reg_count === 3.U){
        io.bram_port.dataIn.get := io.tp_pstate_in.NZCV
      }
      when(sp_reg_count === (SP_REG_N - 1).U){
        state := s_IDLE
      }
    }
    is(write_gen_reg){
      io.bram_port.en := true.B
      wait_r := wait_r + 1.U
      when(wait_r === 0.U){ // 1st clock cycle

      }.otherwise{ // 2nd clock cycle
        g_reg_count := g_reg_count + 1.U
        io.tp_reg_wen := true.B
        when(g_reg_count === (REG_N - 1).U){ // done
          state := write_sp_reg
        }
      }
    }
    is(write_sp_reg){ // TODO: multiple thread
      sp_reg_count := sp_reg_count + 1.U
      // ugly way, do it better, 1 cycle for each special register (32bit value from qemu)
      require(SP_REG_N == 4)
      when(sp_reg_count === 0.U){ // PC
        w_pstate_reg.PC := io.bram_port.dataOut.get
      }
      when(sp_reg_count === 1.U){ // SP
        w_pstate_reg.SP := io.bram_port.dataOut.get
      }
      when(sp_reg_count === 2.U){ // EL
        w_pstate_reg.EL := io.bram_port.dataOut.get
      }
      when(sp_reg_count === 3.U){ // NZCV
        w_pstate_reg.NZCV := io.bram_port.dataOut.get
      }
      when(sp_reg_count === (SP_REG_N - 1).U){
        io.tp_pstate_wen := true.B
        state := s_IDLE
      }
    }

  }
}
