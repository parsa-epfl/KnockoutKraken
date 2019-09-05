// See LICENSE.txt for license details.

package protoflex

import chisel3._
import chisel3.util._
import common.PROCESSOR_TYPES._


class VAddr extends Bundle {
  val tag  = UInt(TLB_TAG.W)
  val set   = UInt(TLB_SZ.W)
  val offset = UInt(PG_OFFSET.W)
}

class PAddr extends Bundle {
  val pn = UInt(PPN.W)
  val offset = UInt(PG_OFFSET.W)
}

class TLBUnitIOLegacy extends Bundle
{
  val vaddr = Input(Valid(new VAddr))
  val rw = Input(Bool())
  val hit = Output(Valid(Bool()))
  val paddr = Output(Valid(new PAddr))

  // writing into tlb for testing
  val write_tlb_valid = Input(Bool())
  val write_tlb_entry  = Input(UInt(TLB_SZ.W))
}

class TLBUnitLegacy  extends Module {
  val io = IO(new TLBUnitIOLegacy)

  // alias
  val set = io.vaddr.bits.set
  val tag = io.vaddr.bits.tag
  val rw = io.rw

  // registers to hold valid request
  val set_reg = Reg(set.cloneType)
  val tag_reg = Reg(tag.cloneType)
  val rw_reg = Reg(rw.cloneType)

  when(io.vaddr.valid) {
    set_reg := set
    tag_reg := tag
    rw_reg := rw
  }

  // tag ram
  val tag_mem = Module(new SRAM(addr_size=TLB_SZ, data_size=TLB_TAG))
  tag_mem.io.addr_r := set
  tag_mem.io.addr_w := set
  tag_mem.io.data_in := tag
  tag_mem.io.wen := io.write_tlb_valid
  val tag_out = tag_mem.io.data_out

  // data ram
  val data_mem = Module(new SRAM(addr_size=TLB_SZ, data_size=PPN))
  data_mem.io.addr_r := set
  data_mem.io.addr_w := set
  data_mem.io.data_in := io.write_tlb_entry
  data_mem.io.wen := io.write_tlb_valid

  // valid and dirty Reg
  val valid = RegInit(0.U(TLB_ENTRIES.W))
  val dirty = RegInit(0.U(TLB_ENTRIES.W))

  // writing tlb entry for testing
  when(io.write_tlb_valid) {
    printf("write tlb tag %x set %x \n", tag, set)
    valid:= valid.bitSet(set, true.B)
  }

  // setting default values of output
  io.paddr.valid := false.B
  io.paddr.bits.pn := data_mem.io.data_out
  io.paddr.bits.offset := io.vaddr.bits.offset // TODO remove this

  //FSM
  val s_IDLE :: s_TAG_CHECK :: Nil = Enum(2)
  val state = RegInit(s_IDLE)

  // hit or miss
  val hit = io.hit.bits
  hit := (tag_out === tag_reg) && valid(set_reg)
  io.hit.valid := false.B

  switch(state) {
    is(s_IDLE) {
      when(io.vaddr.valid & !io.write_tlb_valid){ // TODO second condition only for testing
        state := s_TAG_CHECK
      }
    }
    is(s_TAG_CHECK) {
      io.hit.valid := true.B
      when(hit){
        printf("** TLB hit \n")
        state := s_IDLE
        io.paddr.valid := true.B

        when(!rw_reg.toBool()){
          dirty := dirty.bitSet(set_reg, true.B)
        }
      }.otherwise{
        printf("** TLB miss \n")
        state := s_IDLE
      }
    }
  }
}

class SRAM(addr_size:Int, data_size:Int) extends Module{
  val io = IO(new Bundle{
    val addr_r = Input(UInt(addr_size.W))
    val addr_w = Input(UInt(addr_size.W))
    val wen = Input(Bool())
    val data_in = Input(UInt(data_size.W))
    val data_out = Output(UInt(data_size.W))
  })

  val mem = SyncReadMem(1<<addr_size, UInt(data_size.W))
  when (io.wen) {
    mem.write(io.addr_w, io.data_in)
  }
  io.data_out := mem.read(io.addr_r, true.B)
}
