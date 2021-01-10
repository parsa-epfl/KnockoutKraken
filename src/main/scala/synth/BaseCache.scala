package armflex.synth_wrapper

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

import armflex.cache._
import armflex.util._

import collection.mutable.ArrayBuffer

class BaseCacheWrapper extends MultiIOModule{
  val cacheParameter = new CacheParameter(
    1024, 4, 512, 30, 4, false
  )
  val lruCore = () => new PseudoTreeLRUCore(4)
  val u_cache = Module(BaseCache.generateCache(cacheParameter, lruCore))

  val outputHandshake = Cat(
    // u_cache.backendReadReply_i.ready,
    u_cache.backend_request_o.valid,
    u_cache.frontend_reply_o.valid,
    u_cache.frontend_request_i.ready,
    //u_cache.packet_arrive_o.valid
  )

  val ports = Array(
    // (u_cache.backendReadReply_i.bits, false),
    (u_cache.backend_request_o.bits, true),
    (u_cache.frontend_reply_o.bits, true),
    (u_cache.frontend_request_i.bits, false),
    //(u_cache.packet_arrive_o.bits, true),
    (outputHandshake, true)
  )

  val bits = ports.map({x =>
    math.ceil(x._1.getWidth / 32.0).toInt
  })

  val readOnlyBits = new ArrayBuffer[Boolean]
  ports.zip(bits).foreach({ x=>
    readOnlyBits ++= Seq.fill(x._2)(x._1._2)
  })
  
  implicit val regFileConfig = new AxiMemoryMappedRegFileConfig(
    readOnlyBits.size, 
    readOnlyBits, 
    Seq.fill(readOnlyBits.length)(false)
  )
  val u_reg_file = Module(new AxiMemoryMappedRegFile())

  var index = 0

  u_reg_file.io.regsInput := 0.U.asTypeOf(u_reg_file.io.regsInput.cloneType)

  ports.zip(bits).foreach({x => 
    if(x._1._2){
      // output port
      val subwire = Wire(UInt((x._2 * 32).W))
      subwire := x._1._1.asUInt()
      subwire.asBools().grouped(32).foreach({ x=>
        u_reg_file.io.regsInput(index) <> VecInit(x).asUInt()
        index += 1
      })
    } else {
      val subwire = Cat(u_reg_file.io.regsOutput.slice(index, index + x._2))
      index += x._2
      x._1._1 <> subwire.asTypeOf(x._1._1.cloneType)
    }
  })

  val axi = IO(u_reg_file.io.axiLite.cloneType)

  axi <> u_reg_file.io.axiLite

  val inputHandshake = Cat(
    // u_cache.backendReadReply_i.valid,
    u_cache.backend_request_o.ready,
    u_cache.frontend_request_i.valid,
  )

  // u_cache.backendReadReply_i.valid := axi.awvalid
  u_cache.backend_request_o.ready := axi.rready
  u_cache.frontend_request_i.valid := axi.arvalid
}

object BaseCacheWrapperApp extends App{
  (new ChiselStage).emitVerilog(new BaseCacheWrapper())
}