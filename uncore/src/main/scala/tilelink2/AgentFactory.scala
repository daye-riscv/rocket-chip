// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import cde.Parameters
import ProtocolTransactionTypes._

abstract class TileLinkAgentFactory {
  def getClientParams(
    realm: TileLinkRealmParameters,
    subclients: Seq[TileLinkClientParameters]): Option[TileLinkClientParameters]
//    = { memoize(clients.map(_.getClientParams())) }

  def getManagerParams(
    realm: TileLinkRealmParameters,
    submanagers: Seq[TileLinkManagerParameters]): Option[TileLinkManagerParameters]
//    = { memoize(managers.map(_.getManagerParams())) }

//def autoConnect() = (getClientParams(), getManagerParams(), degreeCheck())
}

abstract class TileLinkManagerFactory {
  def getClientParams(
    realm: TileLinkRealmParameters,
    subclients: Seq[TileLinkClientParameters]): Option[TileLinkClientParameters] = {
      assert(false, "Managers don't have client parameters.")
      None
  }
}


abstract class TileLinkSlaveDevice(implicit p: Parameters) extends TileLinkModule()(p) {
  val io = new TileLinkBundle with HasTileLinkIO {
    val nClientPorts = 0
    val nManagerPorts = 1
  } 

  def load(v: Vec[UInt], acq: Acquire): UInt = {
    val ldBits = v.head.getWidth
    require(isPow2(ldBits) && ldBits >= 8)
    val ldBytes = ldBits/8
    val inBits = tlPhysicalDataWidth
    val inBytes = tlPhysicalDataBytes
    val ratio = inBits/ldBits
    val addr = acq.full_addr()

    if (ldBits > inBits) {
      (v(addr(log2Up(ldBytes*v.size)-1, log2Up(ldBytes))) >>
         addr(log2Up(ldBytes)-1,        log2Up(inBytes))
          )(inBits-1, 0)
    } else {
      val row = for (i <- 0 until v.size by ratio)
        yield Cat(v.slice(i, i + ratio).reverse)
      if (row.size == 1) row.head
      else Vec(row)(addr(log2Up(ldBytes*v.size)-1,log2Up(inBytes)))
    }
  }

  def store(v: Vec[UInt], acq: Acquire): UInt = {
    val stBits = v.head.getWidth
    require(isPow2(stBits) && stBits >= 8)
    val stBytes = stBits/8
    val inBits = tlPhysicalDataWidth
    val inBytes = tlPhysicalDataBytes
    val ratio = inBits/stBits
    val addr = acq.full_addr()
    val rdata = load(v, acq)
    val wdata = (acq.data & acq.full_wmask()) | (rdata & ~acq.full_wmask())

    if (stBits <= inBits) {
      val word =
        if (ratio >= v.size) UInt(0)
        else addr(log2Up(stBytes*v.size)-1,log2Up(inBytes))
      for (i <- 0 until v.size) {
        when (acq.a_type === PUT && word === UInt(i/ratio)) {
          v(i) := wdata >> UInt(inBits * (i % ratio))
        }
      }
    } else {
      val i = addr(log2Up(stBytes*v.size)-1,log2Up(stBytes))
      val mask = FillInterleaved(inBits, UIntToOH(addr(log2Up(stBytes)-1,log2Up(inBytes))))
      v(i) := (wdata & mask) | (v(i) & ~mask)
    }
    rdata
  }
      
}

/*
// terminal client
class RocketFactory(cacheLines: Int)(implict p: Parameters) extends TileLinkAgentFactory {
  def getClientParams(realm: TileLinkRealmParameters, subclients: Seq[TileLinkClientParameters]) = {
    assert (subclients.empty())
    TileLinkClientParameters(log2Up(cacheLineSize/8), nMSHRs, true)
  }
  class Rocket(realm: TileLinkRealmParameters, clients: Seq[TileLinkClientParameters], managers: Seq[TileLinkManagerParameters]) {
    val io = { }
    wires
  }
}
*/

/*
// terminal manger
class BramSlaveFactory(nIOs: Int)(implicit p: Parameters) extends TileLinkAgentFactory {
  val myAddress = AddressAssignment(None, nIOs)
  def getManagerParams(
    realm: TileLinkRealmParameters, submanagers: Seq[TileLinkManagerParameters]): TileLinkManagerParameters = {
    assert (submanagers.empty())
    TileLinkManagerParameters(false, Seq((myAddress, TileLinkRegionParameters(), )
  }
}
*/

/*
// one open client and one open manager
class UncoreFactory(nChannels: Int, nClients: Int)(implicit p: Parameters) extends TileLinkAgentFactory {
  val L1toL2  = p(TLKey("L1toL2"))
  val L2toMem = p(TLKey("L2toMem"))
  val MMIO    = p(TLKey("MMIO"))
  val preMMIO = MMIO.copy(dataBits=L2toM2m.dataBits)
  
  // p has the realm in it
  val L2 = new L2Factory(L1toL2, L2toMem) // TLId
  val AXI = Seq.tabulate(nChannels) { new AXIFactory(L2toMem) }
  val decoherence = new Decoherence(L1toL2, preMMIO)
  val widthAdapter = new WidthAdapter(preMMIO, MMIO)
  val mmio = new XbarFactory(MMIO)
  val topbar = new XbarFactory(L1toL2) // what realms on their ports?
  val bootROM = new BootROMFactory(MMIO)
  val rtc = new RTCFactory(MMIO)
  val debug = new DebugFactory(MMIO)
  
  // LONG-hand: tl(topbar.clientPort(0), l2.managerPort(0))
  
  // tl(clientPort[s], managerPort[s])
  // TileLinkAgentFactory implements ClientPorts and ManagerPorts
  
  clients(nClients) // somehow we needed to say the realms here!
    .map(tl(_, topbar))
  tilelink(topbar, l2)
  tl(topbar, decoherence)
  tl(decoherence, widthAdapter)
  tl(widthAdapter, mmio)
  axi.map(tl(l2, _))
  tl(mmio, rtc) // assert that realms of the ports are equal
  tl(mmio, debug)
  tl(mmio, managers(1))
  
  val ioMaker = p("BusConfig")(l2, mmio)
  
  class Uncore extends TileLinkModule {
    val (clientParams, managerParams) = autoConnect() // check connectivity being exactly once
    val io = ioMaker() : TopIO with HasTileLinkIO
    (io.mem zip topbar.instantiate().io.clients) foreach { (m, t) => m <> t }
  }
  def instantiate() = memoize(Module(new Uncore))
}
*/

/*
abstract class XbarFactory()(implicit p: Parameters) extends TileLinkAgentFactory {
  // outside params?

  class Xbar() extends TileLinkModule {
    val (clientParams, managerParams) = autoConnect()
    val io = new Bundle with HasTileLinkIO
    
    val amap = TileLink2.synthesize_map(submanagers.map(_.addressMap)) // asserts no more Nones
    prefix_sum = nSources
    // xbar for acquire = remap the client_ids + prefix_sum(i)
    // fan-out probes based on source_id
    // fan-out releases by address, remap client_id
    // fan-out grants based on client_id
    // finished based on manager_id
  }
  
  def getClientParams() = memoized {
  lazy val clientParams =
    TileLinkClientParameters(
      subclients.map(_.maxXfrSize).max,
      subclients.map(_.nSources).sum,
      subclients.map(_.mayAcquire).reduce(_||_))
  }
  
  def getManagerParams() memoized {
    val all  =  managers.map(_.supportsAcquire).reduce(_&&_)
    val none = !managers.map(_.supportsAcquire).reduce(_||_)
    assert (all || none)
    
    TileLinkManagerParameters(
      all,
      submanagers.map(_.addressMap),
      subclients.map(_.nSinks).sum)
  }
  
  def instatiate()// = memoize(Module(new Xbar))
}
*/

/*
class DMAFactory() {
  def getClientParams() memoized {
    ClientParams = ...
  }
  def getManagerParams() memoized {
    params = ...
  }
}
*/

// caches: 1-1 TL, cross realms (updates: policy, acquireSize, dataBits), maps !uncacheable=>cacheable, supportsAcquire=true, !uncachable_atomics=self, nSources=refill MSHRs, maxXfrSize, nCaches=1
// tracker: broadcast hub just probes all caches, realm changes policy, sourceToCache, uncached=>tracked
// uncachedAdaptor: connect Acquire=>Get, Release=>Put (and assert only VoluntaryRelease), drops Finishes, sets SupportsAcquire=true
// width adaptor: crosses realms
// atomic adaptor: ???? constraints ????
// SourceCompressor: rewrites source_ids to fit into AXI, must use counters for # rewritten
// WmaskAligner: splits sub-beat get/puts, doesn't any parameters
// SingleBeat: fragment get/puts, no parameters changed
// UncacheableAdaptor: asserts a_type!=acquire, !release, no other changes

