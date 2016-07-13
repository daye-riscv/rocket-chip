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

  val oacq_wmask = {
    import tilelink.Acquire._
    val full = ~UInt(0, tlWmaskWidth)
    //val partial = iacq.bits.op_size(), iacq.bits.addr_byte() TODO for small Gets?
    MuxLookup[UInt,UInt](iacq.bits.a_type, iacq.bits.wmask(), Seq(
      getType       -> full,
      getBlockType  -> full,
      putType       -> iacq.bits.wmask(),
      putBlockType  -> iacq.bits.wmask(),
      putAtomicType -> iacq.bits.wmask(),
      getPrefetchType -> full,
      putPrefetchType -> full))
  }

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
      putPrefetchType -> tilelink2.HintPolicySpecific(should = true, write = true))).toBits
  } 

  def wmaskToSize(acq: tilelink.AcquireMetadata): (UInt, UInt) = {
    val (_, put_offset, put_size) = wmaskToSizeHelper((~acq.wmask()).toBools, 0)
    (put_offset, put_size)
  }

  def wmaskToSizeHelper(all_inside_0: Seq[Bool], defsize: Int): (Seq[Bool], UInt, UInt) = {
    val len = all_inside_0.size
    if (len == 1) {
      (Seq(Bool(true)), UInt(0), UInt(defsize))
    } else {
      val sub_inside_0 = Seq.tabulate (len/2) { i => all_inside_0(2*i) && all_inside_0(2*i+1) }
      val (sub_outside_0, sub_offset, sub_size) = wmaskToSizeHelper(sub_inside_0, defsize+1)
      val all_outside_0 = Seq.tabulate (len) { i => sub_outside_0(i/2) && all_inside_0(i^1) }
      val odd_outside_0 = Seq.tabulate (len/2) { i => all_outside_0(2*i+1) }
      val odd_outside = odd_outside_0.reduce (_ || _)
      val all_outside = all_outside_0.reduce (_ || _)
      val offset = Cat(sub_offset, odd_outside.toBits)
      val size = Mux(all_outside, UInt(defsize), sub_size)
      (all_outside_0, offset, size)
    }
  }

  val (put_offset, put_size) = wmaskToSize(iacq.bits)
  val lg2CacheBlockBytes = log2Up(tlCacheBlockBytes)
  val is_multibeat = iacq.bits.hasMultibeatData()
  val oacq_size = {
    import tilelink.Acquire._
    MuxLookup(iacq.bits.a_type, UInt(0), Seq(
      getType       -> iacq.bits.op_size(),
      getBlockType  -> UInt(lg2CacheBlockBytes),
      putType       -> put_size,
      putBlockType  ->  UInt(lg2CacheBlockBytes),
      putAtomicType -> iacq.bits.op_size(),
      getPrefetchType -> UInt(lg2CacheBlockBytes),
      putPrefetchType -> UInt(lg2CacheBlockBytes)))
  } 

  val oacq_addr = iacq.bits.full_addr() |
                    Mux(iacq.bits.a_type === tilelink.Acquire.putType, put_offset, UInt(0))

  iacq.ready := oacq.ready
  oacq.valid := iacq.valid
  oacq.bits := tilelink2.Acquire(
    a_type = oacq_atype,
    source = Cat(iacq.bits.client_id, iacq.bits.client_xact_id),
    addr = oacq_addr,
    wmask = oacq_wmask,
    union = oacq_union,
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
