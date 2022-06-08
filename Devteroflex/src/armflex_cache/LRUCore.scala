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
  core.io.access_i := index_i.bits
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
sealed abstract class LRUCore(wayNumber: Int, val encodingWidth: Int) extends Module{
  val wayWidth = log2Ceil(wayNumber)
  final val io = IO(new Bundle{
    // the request
    val access_i = Input(UInt(wayWidth.W))
    // lru out
    val lru_o = Output(UInt(wayWidth.W)) // lru_o should be only determined by encodings_i since it points to the current available place before the request is processed.

    // encoding in and out
    val encoding_i = Input(UInt(encodingWidth.W)) // the input encoding bits
    val encoding_o = Output(UInt(encodingWidth.W)) // the encoding bits that updated by the request. 
  })
}


object PseudoTreeLRU {
  def apply(currEncodingBits: UInt, access: UInt): (UInt, UInt) = {
    val encodingSize = currEncodingBits.getWidth
    val accessSize = access.getWidth
    if(accessSize == 1){
      // only one bit is enough to distinguish two elements.
      assert(encodingSize == 1)
    } else {
      assert(log2Ceil(encodingSize) == accessSize)
    }

    val currEnconding = WireInit(VecInit(currEncodingBits.asBools))
    val nextEncoding = WireInit(currEnconding)
    val lruBits = Wire(access.cloneType)

    def getNextEncoded(half: Int, currIdx: Int, lru: Int, currPath: Seq[(Int, Boolean)]): Unit = {
      if(half == 1) {
        when(access === (lru + half).U) {
          (currPath :+ (currIdx, true)).foreach { case (idx, dir) => nextEncoding(idx) := (!dir).B }
        }.elsewhen(access === lru.U) {
          (currPath :+ (currIdx, false)).foreach { case (idx, dir) => nextEncoding(idx) := (!dir).B }
        }
      } else {
        getNextEncoded(half/2, currIdx + half, lru + half, currPath :+ (currIdx, true))
        getNextEncoded(half/2, currIdx + 1, lru, currPath :+ (currIdx, false))
      }
    }

    def getLRU(half: Int, currIdx: Int, lru: Int): Unit = {
      if(half == 1) {
        when(currEnconding(currIdx)) {
          lruBits := (lru + half).U
        }.otherwise {
          lruBits := lru.U
        }
      } else {
        when(currEnconding(currIdx)) {
          getLRU(half/2, currIdx + half, lru + half)
        }.otherwise {
          getLRU(half/2, currIdx + 1, lru)
        }
      }
    }
    getLRU((encodingSize+1)/2, 0, 0)
    getNextEncoded((encodingSize+1)/2, 0, 0, Seq())

    (lruBits, nextEncoding.asUInt)
  }
}

/**
 *  Pseudo tree LRU updating logic. Implemented in recursive function instead of module.
 * 
 *  Tree LRU is appreciated when the tlbAssociativity greater than 4, sel can be normal in L1 TLB.
 */ 
class PseudoTreeLRUCore(wayNumber: Int) extends LRUCore(wayNumber, wayNumber - 1){
  //assert(isPow2(wayNumber))
  val (lru, encoded) = PseudoTreeLRU(io.encoding_i, io.access_i)
  io.lru_o := lru
  io.encoding_o := encoded
}

/**
 *  Real LRU updating logic implemented by matrix. 
 */ 
class MatrixLRUCore(wayNumber: Int) extends LRUCore(wayNumber, wayNumber * (wayNumber - 1)){
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
        when(i.U === io.access_i) {
          updatedMatrix(i)(j) := true.B
        }.elsewhen(j.U === io.access_i) {
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
