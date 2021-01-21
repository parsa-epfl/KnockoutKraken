package armflex.demander.peripheral

import chisel3._
import chisel3.util._

class MemoryRequestPacket(
  addressWidth: Int = 32,
  dataWidth: Int = 32
) extends Bundle {
  val addr = UInt(addressWidth.W)
  val data = UInt(dataWidth.W)
  val w_v = Bool()
  val w_mask = UInt((dataWidth / 8).W)

  override def cloneType: this.type = new MemoryRequestPacket(addressWidth, dataWidth).asInstanceOf[this.type]
}

/**
 * If a bundle can be asscessed by the 32bit core, it should provide methods to serialize into and deserialize from a vector contains 32bit elements
 */ 
abstract class SoftwareControlledBundle extends Bundle {
  /**
   * Serialize the packet itself.
   * 
   * @param width the width of the core. Usually we take it 32bit.
   * @return the serialization result.
   */ 
  def asVec(width: Int): Vec[UInt]

  /**
   * Deserialize from a vector.
   * 
   * @param f the vector
   * @return a decoded packet
   */ 
  def parseFromVec(f: Vec[UInt]): this.type

  def ceilAfterDivide(dividend: Int, divisor: Int): Int = {
    return dividend / divisor + (if (dividend % divisor != 0) 1 else 0)
  }
}

/**
 * A memory-mapped queue. You can pop an element from the queue by memory operation.
 * 
 * @param t the Chisel type stored in the queue.
 * @param addressWidth the width of the address line.
 * @param dataWidth the width of the data line.
 * 
 * @note synchronized read
 */ 
class MMReadQueue[T <: SoftwareControlledBundle](
  t: T,
  addressWidth: Int = 32,
  dataWidth: Int = 32
) extends MultiIOModule {
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(addressWidth, dataWidth))))
  // TODO: Make all the modules read in a sync way.
  val reply_o = IO(Output(UInt(dataWidth.W)))

  val queue_i = IO(Flipped(Decoupled(t.cloneType)))

  assert(t.isWidthKnown)

  val read_vector = Wire(Vec(
    1 + queue_i.bits.asVec(dataWidth).length,
    UInt(dataWidth.W)
  ))

  read_vector(0) := queue_i.valid // reading the first point will let you know whether it's valid.
  queue_i.bits.asVec(dataWidth).zipWithIndex.foreach(x =>
    read_vector(x._2 + 1) := x._1
  )

  // the internal address used to index internal CSRs.
  val subAddressWidth = log2Ceil(read_vector.length)
  val internal_address = request_i.bits.addr(subAddressWidth + log2Ceil(dataWidth / 8) - 1,log2Ceil(dataWidth / 8))
  reply_o := RegNext(read_vector(internal_address))

  val ready_r = Reg(Bool())
  queue_i.ready := ready_r

  when(request_i.valid && request_i.bits.w_v && internal_address === 0.U){
    ready_r := request_i.bits.data(0)
  }.otherwise{
    ready_r := 0.U;
  }
}

/**
 * A memory-mapped queue. You can push an element to the queue by memory operation.
 * 
 * @param t the Chisel type stored in the queue.
 * @param addressWidth the width of the address line.
 * @param dataWidth the width of the data line.
 * 
 * @note synchronized read.
 */ 
class MMWriteQueue[T <: SoftwareControlledBundle](
  t: T,
  addressWidth: Int = 32,
  dataWidth: Int = 32
) extends MultiIOModule {
  val request_i = IO(Flipped(Valid(new MemoryRequestPacket(addressWidth, dataWidth))))
  val reply_o = IO(Output(UInt(dataWidth.W)))

  val queue_o = IO(Decoupled(t.cloneType))

  val write_vector = Reg(Vec(
    1 + queue_o.bits.asVec(dataWidth).length,
    UInt(dataWidth.W)
  ))

  val subAddressWidth = log2Ceil(write_vector.length)
  val internal_address = request_i.bits.addr(subAddressWidth + log2Ceil(dataWidth / 8) - 1,log2Ceil(dataWidth / 8))

  reply_o := RegNext(Mux(
    internal_address === 0.U, 
    queue_o.ready,
    write_vector(internal_address)
  ))

  when(request_i.valid && request_i.bits.w_v){
    write_vector(internal_address) := request_i.bits.data
  }.otherwise {
    write_vector(0) := false.B
  }

  queue_o.valid := write_vector(0)
  queue_o.bits := t.parseFromVec(VecInit(write_vector.slice(1, write_vector.size)))
}

object QueuesVerilogGenerator extends App{
  import chisel3.stage.ChiselStage

  class simple_bundle_t extends SoftwareControlledBundle {
    val x = UInt(100.W)
    val y = UInt(10.W)

    def asVec(width: Int): Vec[UInt] = {
      assert(width == 32)
      val res = Wire(Vec(5,UInt(width.W)))
      x.asBools().grouped(32).zipWithIndex.foreach({
        case (bools, index) => 
        res(index) := VecInit(bools).asUInt()
      })
      x(4) := y
      res
    }

    def parseFromVec(f: Vec[UInt]): this.type = {
      new simple_bundle_t().asInstanceOf[this.type]
    }
  }

  println((new ChiselStage).emitVerilog(new MMReadQueue(new simple_bundle_t)))

  println((new ChiselStage).emitVerilog(new MMWriteQueue(new simple_bundle_t)))
}

