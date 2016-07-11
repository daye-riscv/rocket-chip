// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import junctions._
import cde.{Parameters, Field}
import uncore.coherence.CoherencePolicy
import uncore.tilelink.TLId
import scala.math.max
import RegionType._

case class TLRealmKey(id: String) extends Field[TileLinkRealmParameters]
case class TLClientKey(id: String) extends Field[TileLinkClientParameters]
case class TLManagerKey(id: String) extends Field[TileLinkRealmParameters]

// These are not negotiated, but knobs set by the designer
case class TileLinkRealmParameters(
  policy: CoherencePolicy,
  permGranularityWidth: Int,
  physicalDataWidth: Int)

case class TileLinkSinkParameters(
  sinkIds:      Range,
  regionType:   RegionType,
  allowAtomics: Boolean, // at terminal managers, all regions should agree on yes/no atomics
  matchAddress: Long,
  maskAddress:  Long)

case class TileLinkManagerParameters(sinkView: Seq[TileLinkSinkParameters]) {
  def supportsAcquire: Bool = {
    // tl(,) function checks: assertion: supportsAcquire || nCaches == 0
    //                        assertion: supportsAcquire || !release.valid
    Bool(true)
  }
  def nSinks: Int = sinkView.map(_.sinkIds.count).sum
  //def buildCacheInfo(): UInt => Chilse(RegionType) // UInt = address, not sink_id
  //def buildAtomicInfo(): UInt => Bool
}

case class TileLinkSourceParameters(
  isCache:          Boolean,
  maxTransferSize:  Int,
  sourceIds:        Range) // (1 MSHR = 1 source; 1 client has 1+ sources)
  
case class TileLinkClientParameters(sourceView: Seq[TileLinkSourceParameters]) {
  def maxTransferSize: Int = sourceView.map(_.maxTransferSize).max
  def nSources: Int = sourceView.map(_.sourceIds.count).sum
  def nCaches: Int = sourceView.map(s => if(s.isCache) 1 else 0).sum
  //def makeSourceToCache() = ... 
  //def makeCacheToStartSource() = ...
}

trait HasTileLinkParameters {
  implicit val p: Parameters
  val clientParameters: TileLinkClientParameters = p(TLClientKey(p(TLId)))
  val managerParameters: TileLinkManagerParameters = p(TLManagerKey(p(TLId)))
  val realmParameters: TileLinkRealmParameters = p(TLRealmKey(p(TLId)))

  val tlCoh = realmParameters.policy
  val tlPhysicalDataWidth = realmParameters.physicalDataWidth
  val tlPhysicalDataBytes = tlPhysicalDataWidth / 8
  val tlSubBeatAddrWidth = log2Up(tlPhysicalDataWidth)
  val tlPermGranularityWidth = realmParameters.permGranularityWidth
  val tlSubBlockAddrWidth = log2Up(tlPermGranularityWidth)
  val tlAddrWidth = p(PAddrBits) - tlSubBeatAddrWidth
  val tlCacheBlockAddrWidth = p(PAddrBits) - tlSubBlockAddrWidth

  require(isPow2(tlPhysicalDataWidth)) 
  require(isPow2(tlPermGranularityWidth)) 
  require(tlPhysicalDataWidth <= tlPermGranularityWidth)

  val tlSourceIdWidth = log2Up(clientParameters.nSources)
  val tlSinkIdWidth = log2Up(managerParameters.nSinks)
  val tlTransferSizeWidth = log2Up(clientParameters.maxTransferSize)
  val tlMaxTransferSize = clientParameters.maxTransferSize

  val tlWmaskWidth = tlPhysicalDataBytes

  val tlAcquireUnionWidth = Seq(ProtocolTransactionTypes.acquireHintWidth,
                                AtomicOpCodes.atomicOpCodeWidth,
                                tlCoh.acquireTypeWidth).max
  val tlAcquireTypeWidth = ProtocolTransactionTypes.acquireTypeWidth
  val tlGrantTypeWidth = ProtocolTransactionTypes.grantTypeWidth
}
