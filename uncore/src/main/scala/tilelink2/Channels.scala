// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import cde.Parameters

import ProtocolTransactionTypes._
import PermissionsDeltas._

/** Base traits for all TileLink channels.
  * Directionality of message channel can be used to hook up
  * logical network ports to physical network ports */
abstract class TileLinkChannel(implicit p: Parameters) extends TileLinkBundle()(p) {
  def size(dummy: Option[Any] = None): UInt
}

abstract class ClientToManagerChannel(implicit p: Parameters) extends TileLinkChannel()(p)
abstract class ManagerToClientChannel(implicit p: Parameters) extends TileLinkChannel()(p)
abstract class ClientToClientChannel(implicit p: Parameters) extends TileLinkChannel()(p) // Unused for now

trait MightBeVoluntary {
  def isVoluntary(dummy: Option[Any] = None): Bool
}

/** A single beat of cache block data */
trait HasTileLinkData {
  val data: UInt

  def hasData(dummy: Option[Any] = None): Bool
  def hasMultibeatData(dummy: Option[Any] = None): Bool
}

trait HasAcquireMessageType extends HasTileLinkParameters {
  val a_type: UInt

  def acquireTypeLookup[T <: Data](get: T, put: T, hint: T, atomic: T, cache: T): T = {
    MuxLookup[UInt, T](a_type, get, Seq(
      GET -> get,
      PUT -> put,
      HINT -> hint,
      ATOMIC -> atomic,
      CACHE -> cache))
  }

  def isType(t: UInt) = a_type === t
}

trait HasGrantMessageType extends MightBeVoluntary {
  val g_type: UInt

  def grantTypeLookup[T <: Data](get: T, put: T, hint: T, atomic: T, cache: T, vol: T): T = {
    MuxLookup[UInt, T](g_type, get, Seq(
      GET -> get,
      PUT -> put,
      HINT -> hint,
      ATOMIC -> atomic,
      CACHE -> cache,
      VOLREL -> vol))
  }

  def isType(t: UInt) = g_type === t
  def isVoluntary(dummy: Option[Any] = None): Bool = g_type === VOLREL
}

class HintPolicySpecific extends Bundle {
  val should_allocate = Bool()
  val with_write_permissions = Bool()
}

object HintPolicySpecific {
  def apply(should: Bool, write: Bool): HintPolicySpecific = {
    val h = Wire(new HintPolicySpecific)
    h.should_allocate := should
    h.with_write_permissions := write
    h
  }

  def apply(should: Boolean, write: Boolean): HintPolicySpecific = apply(Bool(should),Bool(write))
}

class CachePolicySpecific extends Bundle {
  val perms = UInt(width = permissionsDeltaWidth)
  val custom = Bool()
  val has_data = Bool()
}

trait HasAcquireUnion extends HasAcquireMessageType {
  val union: UInt

  import AtomicOpCodes._
  def amo_op_code(dummy: Option[Any] = None): UInt =
    acquireTypeLookup(
      get = NOOP,
      put = NOOP,
      hint = NOOP,
      atomic = union(atomicOpCodeWidth-1,0),
      cache = NOOP)
      
  def should_allocate(dummy: Option[Any] = None): Bool =
    acquireTypeLookup(
      get = union(0),
      put = union(0),
      hint = hints().should_allocate,
      atomic = Bool(true),
      cache = Bool(true))

  def contiguous_mask(dummy: Option[Any] = None): Bool = {
    // TODO assert to actually check the mask
    acquireTypeLookup(
      get = Bool(false),
      put = union(1),
      hint = Bool(false),
      atomic = Bool(false),
      cache = Bool(false))
  }

  def policy(dummy: Option[Any] = None): CachePolicySpecific = {
    //assert(a_type === CACHE, "Union isn't storing coherence metadata for this message type.")
    new CachePolicySpecific().fromBits(union)
  }

  def hints(dummy: Option[Any] = None): HintPolicySpecific = {
    //assert(a_type === HINT, "Union isn't storing hints for this message type.")
    new HintPolicySpecific().fromBits(union)
  }
}

/** The Acquire channel is used to intiate coherence protocol transactions in
  * order to gain access to a cache block's data with certain permissions
  * enabled. Messages sent over this channel may be custom types defined by
  * a [[uncore.CoherencePolicy]] for cached data accesse or may be built-in types
  * used for uncached data accesses. Acquires may contain data for Put or
  * PutAtomic built-in types. After sending an Acquire, clients must
  * wait for a manager to send them a [[uncore.Grant]] message in response.
  */
class AcquireMetadata(implicit p: Parameters) extends ClientToManagerChannel
    with HasAcquireMessageType
    with HasAcquireUnion {
  val a_type = UInt(width = tlAcquireTypeWidth)
  val source = UInt(width = tlSourceIdWidth)
  val addr = UInt(width = tlAddrWidth) // aligned to databits
  val wmask = UInt(width = tlWmaskWidth) // within databits
  val union = UInt(width = tlAcquireUnionWidth) // amo_opcode || hints || policy
  val sz = UInt(width = tlTransferSizeWidth) // actual size = 8*2**size

  // size of this message's data (NOT of resp)
  def size(dummy: Option[Any] = None): UInt = {
    acquireTypeLookup(
      get = UInt(0),
      put = sz,
      hint = UInt(0),
      atomic = sz,
      cache = UInt(0))
  }

  def cwf(dummy: Option[Any] = None): Bool = {
    val hint = (sz > UInt(tlPhysicalDataWidth)) && (beatAddrBits() =/= UInt(0))
    acquireTypeLookup(
      get = hint,
      put = Bool(false),
      hint = Bool(false),
      atomic = Bool(false),
      cache = hint)
  }

  def perms(dummy: Option[Any] = None): UInt = {
    acquireTypeLookup[UInt](
      get = N2N,
      put = N2N,
      hint = A2A,
      atomic = N2N,
      cache = policy().perms)
  }

  def beatAddrBits(dummy: Option[Any] = None): UInt = UInt(0) // TODO

  def wmaskToLowAddrBits(dummy: Option[Any] = None): UInt = UInt(0) // TODO

  def addr_beat(dummy: Option[Any] = None): UInt = UInt(0) // TODO

  def full_addr(dummy: Option[Any] = None): UInt = addr << UInt(tlSubBeatAddrWidth) 

  def full_wmask(dummy: Option[Any] = None): UInt =  FillInterleaved(8, wmask) 

  def criticalWordAddr(dummy: Option[Any] = None): UInt = UInt(0) // TODO

  def hasData(dummy: Option[Any] = None): Bool =
    acquireTypeLookup(
      get = Bool(false),
      put = Bool(true),
      hint = Bool(false),
      atomic = Bool(true),
      cache = Bool(false))

  def hasMultibeatData(dummy: Option[Any] = None): Bool = size() > UInt(tlPhysicalDataWidth)
}

/** [[uncore.AcquireMetadata]] with an extra field containing the data beat */
class Acquire(implicit p: Parameters) extends AcquireMetadata with HasTileLinkData {
  val data = UInt(width = tlPhysicalDataWidth)
}

object Acquire {
  def apply(
        a_type: UInt,
        source: UInt,
        addr: UInt,
        wmask: UInt,
        union: UInt,
        sz: UInt,
        data: UInt)
      (implicit p: Parameters): Acquire = {
    val acq = Wire(new Acquire)
    acq.a_type := a_type
    acq.source := source
    acq.addr := addr
    acq.wmask := wmask
    acq.sz := sz
    acq.data := data
    acq
  }
}

/** The Grant channel is used to refill data or grant permissions requested of the 
  * manager agent via an [[uncore.Acquire]] message. It is also used to acknowledge
  * the receipt of voluntary writeback from clients in the form of [[uncore.Release]]
  * messages. There are built-in Grant messages used for Gets and Puts, and
  * coherence policies may also define custom Grant types. Grants may contain data
  * or may be simple acknowledgements. Grants are responded to with [[uncore.Finish]].
  */
class GrantMetadata(implicit p: Parameters) extends ManagerToClientChannel
    with HasGrantMessageType {
  val g_type = UInt(width = tlGrantTypeWidth)
  val source = UInt(width = tlSourceIdWidth)
  val sink = UInt(width = tlSinkIdWidth)
  val sz = UInt(width = tlTransferSizeWidth) // actual size = 8*2**size
  val error = Bool()
  val cwf = Bool()
  val policy = new CachePolicySpecific

  def size(dummy: Option[Any] = None): UInt = {
    grantTypeLookup(
      get = sz,
      put = UInt(0),
      hint = UInt(0),
      atomic = sz,
      cache = UInt(tlPermGranularityWidth),
      vol = UInt(0))
  }

  // mostly same as Acquire but can Grant extra perms on cached
  def perms(dummy: Option[Any] = None): UInt = {
    grantTypeLookup[UInt]( 
      get = N2N,
      put = N2N,
      hint = A2A,
      atomic = N2N,
      cache = policy.perms, // PermUp
      vol = A2A)
  }

  def hasData(dummy: Option[Any]): Bool = {
    grantTypeLookup(
      get = Bool(true),
      put = Bool(false),
      hint = Bool(false),
      atomic = Bool(true),
      cache = policy.has_data,
      vol = Bool(false))
  }

  def hasMultibeatData(dummy: Option[Any] = None): Bool = size() > UInt(tlPhysicalDataWidth)

  def makeFinish(dummy: Option[Any] = None): Finish = {
    val f = Wire(new Finish)
    f.sink := this.sink
    f
  }
}

/** [[uncore.GrantMetadata]] with an extra field containing a single beat of data */
class Grant(implicit p: Parameters) extends GrantMetadata with HasTileLinkData {
  val data  = UInt(width = tlPhysicalDataWidth)
}

object Grant {
  def apply(
        g_type: UInt,
        source: UInt,
        sink: UInt,
        size: UInt,
        error: Bool = Bool(false),
        cwf: Bool = Bool(false),
        data: UInt = UInt(0))
      (implicit p: Parameters): Grant = {
    val gnt = Wire(new Grant)
    gnt.g_type := g_type
    gnt.source := source
    gnt.sink := sink
    gnt.sz := size
    gnt.error := error
    gnt.cwf := cwf
    gnt.policy := UInt(0)
    gnt.data := data
    gnt
  }

  def fromAcquire(
        gnt: DecoupledIO[Grant],
        acq: DecoupledIO[AcquireMetadata],
        sink: UInt = UInt(0),
        error: Bool = Bool(false),
        data: UInt = UInt(0))
      (implicit p: Parameters): Unit = {
    val bits = Grant(
      g_type = acq.bits.a_type,
      source = acq.bits.source,
      sink = sink,
      size = acq.bits.sz, // TODO
      error = error,
      cwf = acq.bits.cwf(), // TODO  
      data = data)(p)
    gnt.valid := acq.valid
    gnt.bits := bits
    acq.ready := TileLinkBeatCounter(gnt.fire(), gnt.bits).done
  }
}

/** The Probe channel is used to force clients to release data or cede permissions
  * on a cache block. Clients respond to Probes with [[uncore.Release]] messages.
  * The available types of Probes are customized by a particular
  * [[uncore.CoherencePolicy]].
  */
class Probe(implicit p: Parameters) extends ManagerToClientChannel{
  val source = UInt(width = tlSourceIdWidth)
  val addr = UInt(width = tlCacheBlockAddrWidth) // aligned to acquire_size, assert (you own the address)
  val perms = UInt(width = permissionsDeltaWidth) // { PermDown, PermSame }
  val hint = Bool() // supply_data: must supply if dirty, requests supply if clean
  //val custom = UInt()
  /*
    Probe                     Release.Mandatory
    "Invalidate" AnyToN    => RWtoN  or RtoN or NtoN
    "Downgrade"  AnyToR    => RWtoR  or RtoR or NtoN
    "Clean"      AnytoSame => RWtoRW or RtoR or NtoN
  */
  def size(dummy: Option[Any] = None) = UInt(0)
}

/** The Release channel is used to release data or permission back to the manager
  * in response to [[uncore.Probe]] messages. It can also be used to voluntarily
  * write back data, for example in the event that dirty data must be evicted on
  * a cache miss.  Releases may contain data or may be simple acknowledgements.
  * Voluntary Releases are acknowledged with [[uncore.Grant Grants]].
  */
class ReleaseMetadata(implicit p: Parameters) extends ClientToManagerChannel {
  val perms = UInt(width = permissionsDeltaWidth) //{ PermDown, PermSame, PermNone, PermForce }
  // Voluntary: PermDown or PermReport or PermForce
  // Mandatory: PermDown or PermReport or PermNone
  val voluntary = Bool()
  val has_data = Bool()
  val addr = UInt(width = tlCacheBlockAddrWidth) // aligned to acquire_size
  val source = UInt(width =  tlSourceIdWidth) // cannot be cache_id, b/c grant needs source_id
  
  def size(dummy: Option[Any] = None): UInt = UInt(log2Up(tlPermGranularityWidth))
  def hasData(dummy: Option[Any] = None): Bool = has_data
  def hasMultibeatData(dummy: Option[Any] = None): Bool = size() > UInt(tlPhysicalDataWidth)
}

/** [[uncore.ReleaseMetadata]] with an extra field containing the data beat */
class Release(implicit p: Parameters) extends ReleaseMetadata with HasTileLinkData {
  val data  = UInt(width = tlPhysicalDataWidth)
}

/** The Finish channel is used to provide a global ordering of transactions
  * in networks that do not guarantee point-to-point ordering of messages.
  * A Finsish message is sent as acknowledgement of receipt of a [[uncore.Grant]].
  * When a Finish message is received, a manager knows it is safe to begin
  * processing other transactions that touch the same cache block.
  */
class Finish(implicit p: Parameters) extends ClientToManagerChannel()(p) {
  val sink = UInt(width = tlSinkIdWidth)

  def size(dummy: Option[Any] = None) = UInt(0)
}

class TileLinkIO(implicit p: Parameters) extends TileLinkBundle()(p) {
  val acquire = new DecoupledIO(new Acquire)
  val probe   = new DecoupledIO(new Probe).flip
  val release = new DecoupledIO(new Release)
  val grant   = new DecoupledIO(new Grant).flip
  val finish  = new DecoupledIO(new Finish)
}
