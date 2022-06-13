package armflex.util

import chisel3._
import chisel3.util._
import chisel3.experimental.{requireIsChiselType, DataMirror, Direction}

class ValidTag[T <: Data](genTag: UInt, genData: Option[T]) extends Bundle {
  val valid = Bool()
  val bits = if(genData.isDefined) Some(genData.get.cloneType) else None
  val tag = genTag.cloneType
}

class Tagged[T <: Data](genTag: UInt, genData: T) extends Bundle {
  val tag = genTag.cloneType
  val data = genData.cloneType
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


/* If both set and clear are risen, it clears the register
 */
class SetClearReg(val width: Int) extends Module {
  val io = IO(new Bundle {
    val set = Input(UInt(width.W))
    val clear = Input(UInt(width.W))
    val value = Output(UInt(width.W))
  })
  val reg = RegInit(0.U(width.W))
  reg := (reg | io.set) & (~io.clear)
  io.value := reg
}

object SetClearReg {
  def apply(width: Int) = {
    val set = WireInit(0.U(width.W))
    val clear = WireInit(0.U(width.W))
    val reg = Module(new SetClearReg(width))
    reg.io.set := set
    reg.io.clear := clear
    
    (reg.io.value, set, clear)
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
