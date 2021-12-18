package instrumentation

import chisel3._
import chisel3.util._
import antmicro.CSR._
import antmicro.Bus._
import arm._
import armflex._
import armflex.util.ExtraUtils._
import armflex.util.{AXIReadMasterIF, AXIReadMultiplexer, AXIWriteMasterIF, AXIWriteMultiplexer, BRAM, BRAMParams, ReadPort, WritePort}
import armflex_mmu.MemoryHierarchyParams
import javax.script.Bindings

class TraceDumpParams(
  val addrW: Int = 12,
  val dataW: Int = 512,
  val traceW: Int = 64,
  val bramSize: Int = 1024,
  val windowSize: Int = 256,
  val burstSize: Int = 1,
)

class TraceDump(val params: TraceDumpParams) extends MultiIOModule {
  val init_addr = IO(Flipped(Decoupled(UInt(params.addrW.W))))
  val trace_data = IO(Flipped(Decoupled(UInt(params.traceW.W))))
  val dram_write_port = IO(Flipped(new WritePort(params.addrW, params.dataW)))

  private val WORDS_PER_BLOCK = params.dataW / params.traceW
  private val bramBufferData = new BRAMParams(
    NB_COL = params.dataW/8, COL_WIDTH = 8, NB_ELE = params.bramSize,
    INIT_FILE = "", false, false, true, false)

  private val b_Buffering :: b_Full :: Nil = Enum(2)
  private val buffer_state = RegInit(b_Buffering)

  private val s_Idle :: s_Packing :: s_Prepare :: s_Write :: Nil = Enum(4)
  private val step = RegInit(s_Idle)

  private val block_buffer = RegInit(VecInit(Seq.fill(WORDS_PER_BLOCK)(0.U.asTypeOf(trace_data.bits))))
  private val curr_word = RegInit(0.U(log2Ceil(WORDS_PER_BLOCK).W))
  private val writes_pending = RegInit(0.U(log2Ceil(params.bramSize).W))

  private val bram = Module(new BRAM()(bramBufferData))
  private val curr_block_in = RegInit(0.U((log2Ceil(params.burstSize) + 1).W))
  private val curr_block_out = RegInit(0.U((log2Ceil(params.burstSize) + 1).W))
  private val bram_in_addr = RegInit(0.U.asTypeOf(bram.portA.ADDR))
  private val bram_out_addr = RegInit(0.U.asTypeOf(bram.portB.ADDR))
  private val bram_full = RegInit(false.B)

  private val init_addr_reg = RegInit(0.U.asTypeOf(init_addr.bits))
  private val curr_addr = RegInit(0.U.asTypeOf(init_addr.bits))

  bram.portA.ADDR := RegNext(bram_in_addr)
  bram.portA.DI := block_buffer.asUInt()
  bram.portA.EN := true.B
  bram.portA.WE := RegNext(Fill(bram.portA.WE.getWidth, !bram_full))

  bram.portB.ADDR := bram_out_addr
  bram.portB.DI := DontCare
  bram.portB.EN := true.B
  bram.portB.WE := false.B

  dram_write_port.data.valid := step === s_Write
  dram_write_port.data.bits := bram.portB.DO
  dram_write_port.req.valid := step === s_Prepare
  dram_write_port.req.bits.w_en := Fill(dram_write_port.req.bits.w_en.getWidth, 1.U.asUInt())
  dram_write_port.req.bits.addr := curr_addr
  dram_write_port.req.bits.burst := params.burstSize.U

  trace_data.ready := buffer_state === b_Buffering && step =/= s_Idle
  switch(buffer_state) {
    is(b_Buffering) {
      when(trace_data.fire) {
        block_buffer(curr_word) := trace_data.bits
        when(curr_word === (WORDS_PER_BLOCK - 1).U) {
          curr_word := 0.U

          when(curr_block_in === (params.burstSize - 1).U) {
            curr_block_in := 0.U
            writes_pending := writes_pending + 1.U
          }.otherwise {
            curr_block_in := curr_block_in + 1.U
          }

          when(bram_full) {
            buffer_state := b_Full
          }.otherwise {
            bram_in_addr := bram_in_addr + 1.U
            when(bram_in_addr + 1.U === bram_out_addr) {
              bram_full := true.B
            }
          }
        }.otherwise {
          curr_word := curr_word + 1.U
        }
      }
    }
    is(b_Full) {
      when(!bram_full) {
        bram_in_addr := bram_in_addr + 1.U
        when(bram_in_addr + 1.U === bram_out_addr) {
          bram_full := true.B
        }
        buffer_state := b_Buffering
      }
    }
  }

  init_addr.ready := step === s_Idle
  switch(step) {
    is(s_Idle) {
      when(init_addr.fire) {
        init_addr_reg := init_addr.bits
        curr_addr := init_addr.bits
        step := s_Packing
      }
    }
    is(s_Packing) {
      when(writes_pending > 0.U) {
        writes_pending := writes_pending - 1.U
        step := s_Prepare
      }
    }
    is(s_Prepare) {
      when(dram_write_port.req.fire) {
        step := s_Write
      }
    }
    is(s_Write) {
      when(dram_write_port.data.fire) {
        bram_out_addr := bram_out_addr + 1.U
        bram_full := false.B

        when(curr_block_out === (params.burstSize - 1).U) {
          curr_block_out := 0.U
          step := s_Packing

          when(curr_addr === init_addr_reg + (params.windowSize - params.burstSize).U) {
            curr_addr := init_addr_reg
          }.otherwise {
            curr_addr := curr_addr + params.burstSize.U
          }
        }.otherwise{
          curr_block_out := curr_block_out + 1.U
          step := s_Prepare
        }
      }
    }
  }

  assert(params.dataW % params.traceW == 0, "Data width must be divisible by the trace width")
  assert(log2Ceil(params.bramSize) == log2Floor(params.bramSize), "BRAM size must be a power of 2")
  assert(params.bramSize % params.burstSize == 0, "BRAM size must be divisible by the burst size")
  assert(bram.portA.ADDR.getWidth == log2Ceil(params.bramSize), "bram_in_addr width != log2(bramSize)")
  assert(bram.portB.ADDR.getWidth == log2Ceil(params.bramSize), "bram_out_addr width != log2(bramSize)")
}

object TraceDumpTopVerilogEmitter extends App {
  val c = new chisel3.stage.ChiselStage

  import java.io._
  import firrtl.options.TargetDirAnnotation

  c.emitVerilog(
    new TraceDump(new TraceDumpParams()
    ), annotations = Seq(TargetDirAnnotation("test/example/")))
}
