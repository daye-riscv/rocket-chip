package uncore.converters

import Chisel._
import cde.Parameters
import uncore.tilelink2
import uncore.tilelink
import uncore.constants

class UncachedTileLink0To2Converter(implicit p: Parameters) extends tilelink2.TileLinkModule()(p)
    with tilelink.HasTileLinkParameters{
  val io = new tilelink2.TileLinkBundle()(p) with tilelink2.HasTileLinkIO {
    val nClientPorts = 1
    val nManagerPorts = 0
    val zero = new tilelink.ManagerTileLinkIO().flip
  } 

  require(tlDataBits == tlPhysicalDataWidth)
  require(tlBlockAddrBits == tlCacheBlockAddrWidth)

  val iacq = Queue(io.zero.acquire, 1)
  val oacq = io.managers.head.acquire
  val ognt = Queue(io.managers.head.grant, 1)
  val ignt = io.zero.grant

  val acqcnt = tilelink2.TileLinkBeatCounter(oacq)
  val gntcnt = tilelink2.TileLinkBeatCounter(ognt)

  val oacq_atype = {
  import tilelink2.ProtocolTransactionTypes._
  import tilelink.Acquire._
    MuxLookup[UInt,UInt](iacq.bits.a_type, GET, Seq(
      getType       -> GET,
      getBlockType  -> GET,
      putType       -> PUT,
      putBlockType  -> PUT,
      putAtomicType -> ATOMIC,
      getPrefetchType -> HINT,
      putPrefetchType -> HINT))
  } 

  def wmaskForNonPuts(acq: tilelink.AcquireMetadata): UInt = UInt(0) // TODO
  val oacq_wmask = Mux(iacq.bits.isPut(), iacq.bits.wmask(), wmaskForNonPuts(iacq.bits))

  def atomicOpCodeHelper(acq: tilelink.AcquireMetadata): UInt = {
    import tilelink2.AtomicOpCodes._
    import constants.MemoryOpConstants._
    MuxLookup[UInt,UInt](iacq.bits.op_code(), NOOP, Seq( 
      M_XA_SWAP -> SWAP,
      M_XA_ADD  -> ADD, 
      M_XA_XOR  -> XOR,
      M_XA_OR   -> OR,
      M_XA_AND  -> AND,
      M_XA_MIN  -> MIN, 
      M_XA_MAX  -> MAX,
      M_XA_MINU -> MINU,
      M_XA_MAXU -> MAXU))
  } 

  val oacq_union = {
    import tilelink.Acquire._
    MuxLookup(iacq.bits.a_type, UInt(0), Seq(
      getType       -> iacq.bits.allocate(),
      getBlockType  -> iacq.bits.allocate(), 
      putType       -> iacq.bits.allocate(), // TODO: contiguous
      putBlockType  -> iacq.bits.allocate(), // TODO: contiguous
      putAtomicType -> atomicOpCodeHelper(iacq.bits),
      getPrefetchType -> tilelink2.HintPolicySpecific(should = true, write = false),
      putPrefetchType -> tilelink2.HintPolicySpecific(should = true, write = true)))
  } 

  val lg2CacheBlockBytes = log2Up(tlCacheBlockBytes)
  def wmaskToSize(acq: tilelink.AcquireMetadata): UInt = UInt(0) // TODO
  val oacq_size = {
    import tilelink.Acquire._
    MuxLookup(iacq.bits.a_type, UInt(0), Seq(
      getType       -> iacq.bits.op_size(),
      getBlockType  -> UInt(lg2CacheBlockBytes),
      putType       -> wmaskToSize(iacq.bits),
      putBlockType  ->  UInt(lg2CacheBlockBytes),
      putAtomicType -> iacq.bits.op_size(),
      getPrefetchType -> UInt(lg2CacheBlockBytes),
      putPrefetchType -> UInt(lg2CacheBlockBytes)))
  } 

  iacq.ready := oacq.ready
  oacq.valid := iacq.valid
  oacq.bits := tilelink2.Acquire(
    a_type = oacq_atype,
    source = Cat(iacq.bits.client_id, iacq.bits.client_xact_id),
    addr = iacq.bits.full_addr(),
    wmask = oacq_wmask,
    union = oacq_union.toBits,
    sz = oacq_size,
    data =  iacq.bits.data
  )
  
  ognt.ready := ignt.ready
  ignt.valid := ognt.valid
  ignt.bits := tilelink.Grant(
    is_builtin_type = Bool(true),
    g_type = ognt.bits.grantTypeLookup(
      get = Mux(ognt.bits.hasMultibeatData(),
              tilelink.Grant.getDataBlockType,
              tilelink.Grant.getDataBeatType),
      put = tilelink.Grant.putAckType,
      hint = tilelink.Grant.prefetchAckType,
      atomic = tilelink.Grant.getDataBeatType,
      cache = tilelink.Grant.getDataBlockType,
      vol = tilelink.Grant.voluntaryAckType),
    dst = ognt.bits.source >> UInt(tlClientXactIdBits),
    client_xact_id = ognt.bits.source(tlClientXactIdBits-1,0),
    manager_xact_id = ognt.bits.sink,
    addr_beat = gntcnt.count,
    data = ognt.bits.data
  )
  
} 
