package armflex.util

import chisel3._
import chisel3.experimental._
import chisel3.util._

class PseudoLRU(NB_ENTRY: Int, NB_ENTRY_WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val idx_1 = Input(Valid(UInt(NB_ENTRY_WIDTH.W)))
    val idx_2 = Input(Valid(UInt(NB_ENTRY_WIDTH.W)))
    val lru_idx = Output(UInt(NB_ENTRY_WIDTH.W))
  })

  val treeNodes = RegInit(0.U(NB_ENTRY.W))
  val treeNodes_next = WireInit(VecInit(treeNodes.asBools))

  def updateTree(sel: UInt, selBit: Int, currNode: Int, treeSize: Int): Unit = {
    assert(selBit+1 == log2Ceil(treeSize))

    val bit = sel(selBit)
    treeNodes_next(currNode) := Mux(bit, false.B, true.B)
    if(selBit != 0) {
      when(bit) {
        updateTree(sel, selBit-1, currNode + treeSize/2, treeSize/2)
      }.otherwise {
        updateTree(sel, selBit-1, currNode + 1, treeSize/2)
      }
    }
  }

  when(io.idx_1.valid) { updateTree(io.idx_1.bits, NB_ENTRY_WIDTH-1, 0, NB_ENTRY) }
  // NOTE Can override some of the tree updates of the other port
  when(io.idx_2.valid) { updateTree(io.idx_2.bits, NB_ENTRY_WIDTH-1, 0, NB_ENTRY) }

  treeNodes := treeNodes_next.asUInt

  val lru = Wire(Vec(NB_ENTRY_WIDTH, Bool()))
  def get_LRU(selBit: Int, currNode: Int, treeSize: Int): Unit = {
    assert(selBit+1 == log2Ceil(treeSize))

    val bit: Bool = treeNodes_next(currNode)
    lru(selBit) := bit
    if(selBit != 0) {
      when(bit) {
        get_LRU(selBit-1, currNode + treeSize/2, treeSize/2)
      }.otherwise {
        get_LRU(selBit-1, currNode + 1, treeSize/2)
      }
    }
  }
  get_LRU(NB_ENTRY_WIDTH-1, 0, NB_ENTRY)

  io.lru_idx := lru.asUInt
}

// Source:
// https://chipress.co/2019/07/09/how-to-implement-true-lru-ii/
class lru_linked_list(NB_ENTRY: Int) extends BlackBox(Map(
  "NB_ENTRY"  -> NB_ENTRY,
  "IDX_WIDTH" -> log2Ceil(NB_ENTRY)
)) with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk = Input(Bool())
    val rst = Input(Bool())

    val acc_en = Input(Bool())
    val acc_idx = Input(UInt(log2Ceil(NB_ENTRY).W))

    val lru_idx = Output(UInt(log2Ceil(NB_ENTRY).W))
  })

  setInline("",s"""
      |module lru_linked_list
      |#( parameter    NO_ENTRY = 8,
      |   parameter    IDX_WIDTH = 3))
      | (
      |     input                           clk,
      |     input                           rst,
      |
      |     input                           acc_en,     // indicating a valid access
      |     input [IDX_WIDTH-1:0]           acc_idx,    // the index to entry being accessed
      |
      |     output logic [IDX_WIDTH-1:0]    lru_idx
      |
      | );
      |
      |     logic [NO_ENTRY-1:0][IDX_WIDTH-1]       prev_idx;
      |     logic [NO_ENTRY-1:0][IDX_WIDTH-1]       next_idx;
      |     logic [IDX_WIDTH-1:0]                   head_idx;
      |     logic [IDX_WIDTH-1:0]                   tail_idx;
      |     logic                                   acc_head, acc_tail, acc_middle;
      |
      |     assign acc_head = acc_en & (acc_idx == head_idx);
      |     assign acc_tail = acc_en & (acc_idx == tail_idx);
      |     assign acc_middle = acc_en & !acc_head & !acc_tail;
      |     assign acc_nonhead = acc_en & !acc_head;
      |
      |     // head_idx logic
      |     always_ff @(posedge clk or posedge rst)
      |         if (rst)
      |             head_idx <= '0;
      |         else if (acc_nonhead)
      |             head_idx <= acc_idx;
      |
      |     // tail_idx logic
      |     always_ff @(posedge clk or posedge rst)
      |         if (rst)
      |             tail_idx <= '1;
      |         else if (acc_tail)
      |             tail_idx <= prev_idx[tail_idx];
      |
      |     genvar i;
      |     generate
      |         for (i = 0; i < NO_ENTRY; i = i + 1) begin
      |             // prev_idx logic
      |             always_ff @(posedge clk or posedge rst)
      |                 if (rst)
      |                     prev_idx[i] <= (i - 1);
      |                 else if (acc_nonhead & (head_idx == i))             // update prev_idx of old head_idx
      |                     prev_idx[i] <= acc_idx;
      |                 else if (acc_middle & (next_idx[acc_idx] == i))     // update prev_idx of acc_idx's next_idx
      |                     prev_idx[i] <= prev_idx[acc_idx];
      |
      |             // next_idx logic
      |             always_ff @(posedge clk or posedge rst)
      |                 if (rst)
      |                     next_idx[i] <= (i + 1);
      |                 else if (acc_nonhead & (acc_idx == i))              // update next_idx of new head_idx
      |                     next_idx[i] <= head_idx;
      |                 else if (acc_middle & (prev_idx[acc_idx] == i))     // update next_idx of acc_idx's prev_idx
      |                     next_idx[i] <= next_idx[acc_idx];
      |
      |         end
      |     endgenerate
      |
      |     // lru_idx assignment
      |     assign lru_idx = tail_idx;
      |
      | endmodule: lru_linked_list
  """.stripMargin)
}
