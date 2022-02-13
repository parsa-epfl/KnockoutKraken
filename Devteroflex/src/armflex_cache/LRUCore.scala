package armflex_cache


import armflex.util._

import chisel3._
import chisel3.util._

/**
 *  Base class of LRU module.
 *  @params lruCore the LRU updating logic
 *  @params params Databank parameters
 */
class LRU[T <: LRUCore](
  params: DatabankParams,
  lruCore: () => T
) extends Module {
  val addr_i = IO(Input(UInt(params.setWidth.W)))
  val index_i = IO(Flipped(ValidIO(UInt(params.wayWidth.W))))
  val lru_o = IO(Output(UInt(params.wayWidth.W)))
  // add an extra stage to store the addr. They will be used for the LRU bits writing back.
  val addr_s1_r = if(params.implementedWithRegister) addr_i else RegNext(addr_i)

  // Connected to the LRU Core
  val core = Module(lruCore())

  implicit val bramParams = new BRAMParams(
    1,
    core.encodingWidth,
    params.setNumber
    )

  val bram = Module(new BRAMorRegister(params.implementedWithRegister))

  bram.portA.EN := true.B
  bram.portA.ADDR := addr_i
  bram.portA.WE := false.B
  bram.portA.DI := 0.U


  core.io.encoding_i := bram.portA.DO
  core.io.lru_i := index_i.bits
  lru_o := core.io.lru_o

  // write back
  bram.portB.EN := index_i.valid
  bram.portB.ADDR := addr_s1_r
  bram.portB.WE := index_i.valid
  bram.portB.DI := core.io.encoding_o
}

/**
 *  the interface of LRU updating logic. Pure combinational logic.
 *  @params wayNumber how many ways this LRU could handle.
 */ 
sealed abstract class LRUCore(wayNumber: Int) extends Module{
  def encodingWidth: Int  // how many bits are needed to store the encoding bits.
  val wayWidth = log2Ceil(wayNumber)
  final val io = IO(new Bundle{
    // the request
    val lru_i = Input(UInt(wayWidth.W))
    // lru out
    val lru_o = Output(UInt(wayWidth.W)) // lru_o should be only determined by encodings_i since it points to the current available place before the request is processed.

    // encoding in and out
    val encoding_i = Input(UInt(encodingWidth.W)) // the input encoding bits
    val encoding_o = Output(UInt(encodingWidth.W)) // the encoding bits that updated by the request. 
  })
}

/**
 *  Pseudo tree LRU updating logic. Implemented in recursive function instead of module.
 * 
 *  Tree LRU is appreciated when the tlbAssociativity greater than 4, sel can be normal in L1 TLB.
 */ 
class PseudoTreeLRUCore(wayNumber: Int) extends LRUCore(wayNumber){
  //assert(isPow2(wayNumber))
  override def encodingWidth: Int = wayNumber - 1
  // lru_o, encoding_o
  def getLRU(startIndex: Int, wayNumber: Int): UInt = {
    val startBit = io.encoding_i(startIndex)
    if(wayNumber == 2){
      return startBit
    } else {
      val sub_encoding = Mux(startBit, getLRU(startIndex + wayNumber/2, wayNumber/2), getLRU(startIndex + 1, wayNumber/2))
      return Cat(startBit, sub_encoding)
    }
  }

  io.lru_o := getLRU(0, wayNumber)

  val updatedEncoding = WireInit(VecInit(io.encoding_i.asBools()))
  def updateEncoding(startIndex: Int, wayNumber: Int): Unit = {
    val wayWidth = log2Ceil(wayNumber)
    val startBit = io.encoding_i(startIndex)
    val judgeBit = io.lru_i(wayWidth-1)
    updatedEncoding(startIndex) := Mux(startBit === judgeBit, ~startBit, startBit)
    if(wayNumber > 2){
      when(judgeBit){
        updateEncoding(startIndex + wayNumber / 2, wayNumber / 2)
      }.otherwise{
        updateEncoding(startIndex + 1, wayNumber / 2)
      }
    }
  }

  updateEncoding(0, wayNumber)

  io.encoding_o := updatedEncoding.asUInt
}

/**
 *  Real LRU updating logic implemented by matrix. 
 */ 
class MatrixLRUCore(wayNumber: Int) extends LRUCore(wayNumber){
  override def encodingWidth: Int = wayNumber * (wayNumber - 1)
  // 1. recover the matrix structure.
  val matrix = WireInit(VecInit(Seq.fill(wayNumber)(VecInit(Seq.fill(wayNumber)(false.B)))))
  var encodingCnt = 0
  for(i <- 0 until wayNumber){
    for(j <- 0 until wayNumber){
      if(i == j){
        matrix(i)(j) := false.B
      } else {
        matrix(i)(j) := io.encoding_i(encodingCnt)
        encodingCnt += 1
      }
    }
  }

  // 2. update the bits accordingly.
  val updatedMatrix = WireInit(VecInit(Seq.fill(wayNumber)(VecInit(Seq.fill(wayNumber)(false.B)))))
  for(i <- 0 until wayNumber){
    for(j <- 0 until wayNumber){
      if(i == j){
        updatedMatrix(i)(j) := false.B
      } else {
        when(i.U === io.lru_i) {
          updatedMatrix(i)(j) := true.B
        }.elsewhen(j.U === io.lru_i) {
          updatedMatrix(i)(j) := false.B
        }.otherwise{
          updatedMatrix(i)(j) := matrix(i)(j)
        }
      }
    }
  }
  
  // 3. output & flatten
  val allZeroRow = VecInit(matrix.map({x =>
    x.asUInt === 0.U
  }))
  io.lru_o := PriorityEncoder(allZeroRow)

  val flattenedMatrix = WireInit(VecInit(Seq.fill(encodingWidth)(false.B)))
  encodingCnt = 0
  
  for(i <- 0 until wayNumber){
    for(j <- 0 until wayNumber){
      if(i != j){
        flattenedMatrix(encodingCnt) := updatedMatrix(i)(j)
        encodingCnt += 1
      }
    }
  }

  io.encoding_o := flattenedMatrix.asUInt
}
