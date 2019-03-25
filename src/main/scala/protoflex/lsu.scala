// See LICENSE.txt for license details.
package protoflex

import chisel3._
import chisel3.util._
import common.DECODE_CONTROL_SIGNALS._
import common.PROCESSOR_TYPES._

// Minst is executed memory instruction
class MInst extends Bundle {
  val res  = DATA_T
  val rd   = REG_T
  val rd_en = C_T
  val tag  = TAG_T
}

// memory request from lsu to cache
class MemReq extends Bundle{
  val addr = UInt(PADDR.W)
  val rw = C_T
  val data = DATA_T
}

// memory response from cache to lsu
class MemRes extends Bundle{
  val data = DATA_T
}

class LoadStoreUnitIO extends Bundle
{
  val dinst = Input(new DInst)
  val minst = Output(Valid(new MInst))
  val rVal1 = Input(DATA_T)
  val rVal2 = Input(DATA_T)
  val pc  = Input(DATA_T)

  // memory interface
  val memReq = Output(Valid(new MemReq))
  val memRes = Input(Valid(new MemRes))

  // write tlb entriles for initial testing
  val write_tlb_entry  = Input(UInt(TLB_SZ.W))
  val write_tlb_vaddr = Input(Valid(new VAddr))
}

/** LoadStoreunit execute memory instructions. It only keep one memory request now.
  * dinst - decoded instruction from issueunit
  * rVal1 & rVal2 - input register values
  * pc - program counter (for pc relative addressing)
  * */
class LoadStoreUnit extends Module
{
  val io = IO(new LoadStoreUnitIO)

  // register l/s instruction
  val dinst_reg = Reg(io.dinst.cloneType)

  // base address as SInt
  val base = io.pc.zext()

  // Offset
  val imm_sign_extened = Wire(SInt(DATA_W))
  imm_sign_extened := io.dinst.imm.asSInt
//  val offset = Mux(io.dinst.imm_en , 0.S(DATA_W), imm_sign_extened)
  val offset = imm_sign_extened

  // address calculation
  val vaddr = Wire(UInt(VADDR.W))
  vaddr := (base + offset).asUInt()
  val vaddr_IO = Wire(new VAddr())
  // TODO do this in pretty way
  vaddr_IO.offset := vaddr(PG_OFFSET-1, 0)
  vaddr_IO.set := vaddr(PG_OFFSET+TLB_SZ-1,PG_OFFSET)
  vaddr_IO.tag := vaddr(VADDR-1, PG_OFFSET+TLB_SZ)

    // data for write
  val data  = WireInit(DATA_X)
  val rw = true.B //TODO make it depend on itype

  // tlb instance
  val tlb = Module(new TLBUnit())
  // test interface to write some entries
  tlb.io.write_tlb_valid := io.write_tlb_vaddr.valid
  tlb.io.write_tlb_entry := io.write_tlb_entry
  tlb.io.vaddr.bits := Mux(io.write_tlb_vaddr.valid, io.write_tlb_vaddr.bits, vaddr_IO)

  tlb.io.vaddr.valid := false.B
  tlb.io.rw := rw

  // mem request interface
  io.memReq.valid := false.B
  io.memReq.bits.addr := tlb.io.paddr.bits.asUInt()
  io.memReq.bits.data := data
  io.memReq.bits.rw := rw


  val res = WireInit(DATA_X)
  when (io.memRes.valid){
    res := io.memRes.bits.data
  }

  // create Minst output
  val minst = Wire(new MInst)
  minst.res := io.memRes.bits.data
  minst.rd  := dinst_reg.rd
  minst.rd_en := dinst_reg.rd_en
  minst.tag := dinst_reg.tag

  // Output
  io.minst.bits := minst
  io.minst.valid := false.B

  // State machine of lsu
  val s_IDLE :: s_TLB_CHK :: s_MEM_REQ :: Nil = Enum(3)
  val state = RegInit(s_IDLE)
  switch (state) {
    is(s_IDLE){
      when(io.dinst.itype === I_LSImm) {
        dinst_reg := io.dinst
        tlb.io.vaddr.valid := true.B // tlb request
        state := s_TLB_CHK
      }
    }
    is(s_TLB_CHK){
      when(tlb.io.hit.valid) {
        when(tlb.io.hit.bits.toBool()){
          io.memReq.valid := true.B // mem request
          state := s_MEM_REQ
        }.otherwise{
          //TODO transplant
          state := s_IDLE
        }
      }
    }
    is(s_MEM_REQ){
      when(io.memRes.valid.toBool()){
        io.minst.valid := true.B
        state := s_IDLE
      }
    }
  }
}
