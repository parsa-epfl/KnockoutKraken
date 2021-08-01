package armflex.util

import chisel3._
import chisel3.util._
import chisel3.experimental.{requireIsChiselType, DataMirror, Direction}

class ValidTag[T <: Data](genTag: UInt, genData: Option[T]) extends Bundle {
  val valid = Bool()
  val bits = genData
  val tag = genTag.cloneType
  override def cloneType: this.type = ValidTag(genTag, genData).asInstanceOf[this.type]
}

class Tagged[T <: Data](genTag: UInt, genData: T) extends Bundle {
  val tag = genTag.cloneType
  val data = genData.cloneType
  override def cloneType: this.type = new Tagged(genTag, genData).asInstanceOf[this.type]
}

object Tagged {
  def apply[T <: Data](genTag: UInt, genData: T): Tagged[T] = {
    val wire = Wire(new Tagged(genTag, genData))
    wire.tag := genTag
    wire.data := genData
    wire
  }
}

object ValidTag {
  def apply[T <: Data](genTag:    UInt, genData: Option[T]): ValidTag[T] = new ValidTag(genTag, genData)
  def apply[T <: Data](genTag:    UInt, genData: T): ValidTag[T] = new ValidTag(genTag, Some(genData))
  def apply[T <: Data](genTag:    UInt): ValidTag[T] = new ValidTag(genTag, None)
  def apply[T <: Data](thidN: Int, genData: T): ValidTag[T] = ValidTag(UInt(log2Ceil(thidN).W), genData)
  def apply[T <: Data](thidN: Int): ValidTag[T] = ValidTag(UInt(log2Ceil(thidN).W), None)
}

class DecoupledTag[T1 <: UInt, T2 <: Data](genTag: T1, genData: T2) extends DecoupledIO[T2](genData) {
  val tag = Output(genTag.cloneType)
  override def cloneType: this.type = DecoupledTag(genTag, genData).asInstanceOf[this.type]
}

object DecoupledTag {
  def apply[T1 <: UInt, T2 <: Data](genTag: T1, genData: T2): DecoupledTag[T1, T2] = new DecoupledTag(genTag, genData)
}

object ExtraUtils {
  implicit class AddMethodsToDecoupled[T <: Data](self: DecoupledIO[T]) {
    def handshake[T <: Data](target: DecoupledIO[T]): Unit = {
      target.ready <> self.ready
      target.valid <> self.valid
    }
    def handshake[T <: Data](target: DecoupledIO[T], cond: Bool): Unit = {
      when(cond) {
        this.handshake(target)
      }
    }
  }

  implicit class AddMethodsToDecoupledTag[T1 <: UInt, T2 <: Data](self: DecoupledTag[T1, T2])
      extends AddMethodsToDecoupled[T2](self) {
    def handshake[T1 <: UInt, T2 <: Data](target: DecoupledTag[T1, T2]) = {
      self.tag <> target.tag
      super.handshake(target)
    }
    def handshake[T1 <: UInt, T2 <: Data](target: DecoupledTag[T1, T2], cond: Bool) = {
      self.tag <> target.tag
      super.handshake(target, cond)
    }

  }

}

/** An I/O Bundle for FlushReg (FlushRegister)
  * @params gen The type of data of the Reg
  */
class FlushRegIO[T1 <: UInt, T2 <: Data](private val genTag: T1, private val gen: T2) extends Bundle {
  val enq = Flipped(DecoupledTag(genTag, gen))
  val deq = DecoupledTag(genTag, gen)

  /** Flush */
  val flush = Input(ValidTag(genTag))
}

class FlushReg[T1 <: UInt, T2 <: Data](private val genTag: T1, private val gen: T2) extends Module() {

  val io = IO(new FlushRegIO(genTag, gen))

  val reg = Reg(gen)
  val tag = Reg(genTag)
  val valid = RegInit(false.B)

  val flush = io.flush.valid && (tag === io.flush.tag)

  val do_enq = WireInit(!valid || io.deq.ready || flush)

  when(do_enq) {
    reg := io.enq.bits
    tag := io.enq.tag
    valid := io.enq.valid
  }
  io.enq.ready := do_enq

  io.deq.bits := reg
  io.deq.tag := tag
  io.deq.valid := valid && !flush
}

class RoundRobinArbiter(arbN: Int) extends Module {

  val io = IO(new Bundle {
    // Ready threads
    val ready = Input(UInt(arbN.W))
    // Next thread to issue
    val next = Decoupled(UInt(log2Ceil(arbN).W))
  })

  def rotateRight[T <: Data](norm: UInt, rot: UInt): UInt = {
    val right = norm >> rot
    val left = norm << ~rot + 1.U;
    val res = left | right
    res(arbN - 1, 0)
  }

  /*
   * valid      At least one thread is ready to be issued
   * curr       Thread with highest priority during round
   * ready_ofst Ready vector shifted such as lsb is the
   *            offset from curr thread to the thread with
   *            highest priority
   * ofst       Offset from curr thread to thread to be issued
   * next       Thread being issued on this cycle if next stage ready
   */
  val valid = io.ready.orR
  val curr = RegInit(0.U(log2Ceil(arbN).W)) // Last thread issued
  val ready_ofst = rotateRight(io.ready, curr)
  val ofst = PriorityEncoder(ready_ofst)
  val next = curr + ofst

  io.next.bits := next
  io.next.valid := valid
  when(io.next.ready && valid) { curr := next + 1.U }
}

class FlushQueueIO[T <: Data](private val gen: T) extends Bundle {
  val enq = Flipped(Decoupled(gen))
  val deq = Decoupled(gen)

  /** Flush */
  val flush = Input(Bool())
}

class FlushQueue[T <: Data](
  gen:     T,
  entries: Int = 2,
  pipe:    Boolean = false,
  flow:    Boolean = false)
    extends Module {

  val io = IO(new FlushQueueIO(gen))

  val ram = Mem(entries, gen)
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)

  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireDefault(io.enq.fire())
  val do_deq = WireDefault(io.deq.fire())

  when(do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when(do_deq) {
    deq_ptr.inc()
  }
  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := ram(deq_ptr.value)

  if (flow) {
    when(io.enq.valid) { io.deq.valid := true.B }
    when(empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when(io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when(io.deq.ready) { io.enq.ready := true.B }
  }

  when(io.flush) {
    enq_ptr.reset()
    deq_ptr.reset()
    maybe_full := false.B

    io.deq.valid := false.B
    io.enq.ready := false.B
  }
}

object FlushQueue {
  def apply[T <: Data](genTag: T, entries: Int): FlushQueue[T] = new FlushQueue(genTag, entries)
  def apply[T <: Data](
    in:      DecoupledIO[T],
    entries: Int = 2,
    pipe:    Boolean = false,
    flow:    Boolean = false,
    flush:   Bool = false.B
  ): DecoupledIO[T] = {
    if (entries == 0) {
      val res = Wire(Decoupled(in.bits.cloneType))
      res.bits := in.bits
      when(flush) {
        res.valid := false.B
        in.ready := false.B
      }.otherwise {
        res.valid := in.valid
        in.ready := res.ready
      }
      res
    } else {
      val u_queue = Module(new FlushQueue(in.bits.cloneType, entries, pipe, flow))
      u_queue.io.enq <> in
      u_queue.io.flush := flush
      u_queue.io.deq
    }
  }
}