package armflex.util

import SoftwareStructs._
import armflex_p.armflex_p
import armflex_p.ArmflexArchState_p
import armflex_p.ArmflexCommitTrace_p
import com.google.protobuf.ByteString

object ArmflexProtoBuf {
  private def state2proto(state: PState): ArmflexArchState_p =
    ArmflexArchState_p(
      state.xregs map toLong,
      toLong(state.pc),
      toLong(state.sp),
      state.nzcv
    )

  private def trace2proto(trace: CommitTrace): ArmflexCommitTrace_p =
    ArmflexCommitTrace_p(
      Some(state2proto(trace.state)),
      toInt(trace.inst),
      trace.mem_addr.map(toLong),
      trace.mem_data.map(toLong),
      ByteString.copyFrom(trace.inst_block.toByteArray),
      trace.mem_block.map(bigint => ByteString.copyFrom(bigint.toByteArray)),
    )

  private def proto2state(state_p: ArmflexArchState_p): PState = PState(
    state_p.xregs.map(asU _).toList,
    asU(state_p.pc),
    asU(state_p.sp),
    state_p.nzcv
  )

  private def proto2trace(trace_p: ArmflexCommitTrace_p): CommitTrace =
    CommitTrace(
      proto2state(trace_p.state.get),
      asU(trace_p.inst),
      BigInt(0.toByte +: trace_p.instBlockData.toByteArray.reverse),
      trace_p.memAddr.map(asU _).toList,
      trace_p.memData.map(asU _).toList,
      trace_p.memBlockData.map(bytestring => BigInt(0.toByte +: bytestring.toByteArray.reverse)).toList
    )

  def proto2state(proto: Array[Byte]): PState = {
    val state_p = ArmflexArchState_p.parseFrom(proto)
    proto2state(state_p)
  }

  def state2protobuf(state: PState): Array[Byte] = {
    val state_p = state2proto(state)
    state_p.toByteArray
  }

  def proto2commitTrace(proto: Array[Byte]): CommitTrace = {
    val trace_p = ArmflexCommitTrace_p.parseFrom(proto)
    proto2trace(trace_p)
  }

  def commitTrace2protobuf(trace: CommitTrace): Array[Byte] = {
    val trace_p = trace2proto(trace)
    trace_p.toByteArray
  }
}
