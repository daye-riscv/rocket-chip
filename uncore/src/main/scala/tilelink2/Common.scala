// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import cde.Parameters

/** Generic range representation */
case class Range(begin: Int, end: Int) {
  // [0,0) = empty range
  require(end >= begin)
  def count: Int = if(end == 0) 0 else (end - begin + 1)
}

/** Options for memory regions */
object RegionType {
  sealed trait RegionType
  case object CACHED      extends RegionType
  case object TRACKED     extends RegionType
  case object UNCACHED    extends RegionType
  case object UNCACHEABLE extends RegionType
  val cases = Seq(CACHED, TRACKED, UNCACHED, UNCACHEABLE)
}

/** Logical descriptions of permissions transitions */
object PermissionsDeltas {
  sealed class Permissions(bRep: String) {
    def ===(that: Permissions): Bool = this.toUInt === that.toUInt
    def ===(that: UInt): Bool = UInt(this.bRep) === that
    def toUInt: UInt = UInt(this.bRep)
  }
  case object NONE extends Permissions("b00")
  case object READ extends Permissions("b01")
  case object READWRITE extends Permissions("b10")
  case object ANY extends Permissions("b11")
  val permissionsWidth = 2

  sealed class PermissionsDelta(from: Permissions, to: Permissions) {
    def toUInt: UInt = Cat(from.toUInt, to.toUInt)
    def ===(that: PermissionsDelta): Bool = this.toUInt === that.toUInt
  }
  case object RW2R extends PermissionsDelta(READWRITE, READ)
  case object RW2N extends PermissionsDelta(READWRITE, NONE)
  case object R2N extends PermissionsDelta(READ, NONE)
  case object N2R extends PermissionsDelta(NONE, READ)
  case object N2RW extends PermissionsDelta(NONE, READWRITE)
  case object R2RW extends PermissionsDelta(READ, READWRITE)
  case object RW2RW extends PermissionsDelta(READWRITE, READWRITE)
  case object R2R extends PermissionsDelta(READ, READ)
  case object N2N extends PermissionsDelta(NONE, NONE)
  case object A2N extends PermissionsDelta(ANY, NONE)
  case object A2R extends PermissionsDelta(ANY, READ)
  case object A2A extends PermissionsDelta(ANY, ANY)

  implicit def pd2UInt(x: PermissionsDelta) = x.toUInt
  implicit def pdTuple2UInt[T](x: (PermissionsDelta, T)) = (x._1.toUInt, x._2)

  val permissionsDeltaWidth = permissionsWidth * 2

  def permsDown   = Vec[UInt](RW2R, RW2N, R2N)
  def permsUp     = Vec[UInt](N2R, N2RW, R2RW)
  def permsPeek   = Vec[UInt](R2R, RW2RW)
  def permsNone   = Vec[UInt](N2N)
  def permsForce  = Vec[UInt](A2N, A2R)
  def permsSame   = Vec[UInt](A2A)

  def permsAcquire = permsNone ++ permsSame ++ permsUp
  def permsGrant = permsAcquire
  def permsProbe = permsForce ++ permsSame
  def permsRelease = permsDown ++ permsPeek ++ permsNone ++ permsForce
}

object AtomicOpCodes {
  sealed abstract class AtomicOpCode(val bRep: String) {
    def toUInt: UInt = UInt(this.bRep)
    def ===(that: AtomicOpCode): Bool = UInt(this.bRep) === UInt(that.bRep)
    def ===(that: UInt): Bool = UInt(this.bRep) === that
  }
  case object NOOP extends AtomicOpCode("b0000")
  case object SWAP extends AtomicOpCode("b0100")
  case object ADD  extends AtomicOpCode("b1000")
  case object XOR  extends AtomicOpCode("b1001")
  case object OR   extends AtomicOpCode("b1010")
  case object AND  extends AtomicOpCode("b1011")
  case object MIN  extends AtomicOpCode("b1100")
  case object MAX  extends AtomicOpCode("b1101")
  case object MINU extends AtomicOpCode("b1110")
  case object MAXU extends AtomicOpCode("b1111")

  val atomicOpCodeWidth = 4

  implicit def amoOpCode2UInt(x: AtomicOpCode) = x.toUInt
  implicit def amoTuple2UInt[T](x: (AtomicOpCode, T)) = (x._1.toUInt, x._2)
}

object ProtocolTransactionTypes {
  sealed abstract class TransactionType(val bRep: String) {
    def toUInt: UInt = UInt(this.bRep)
    def ===(that: TransactionType): Bool = UInt(this.bRep) === UInt(that.bRep)
    def ===(that: UInt): Bool = UInt(this.bRep) === that
  }
  case object GET extends TransactionType("b000")
  case object PUT extends TransactionType("b001")
  case object HINT extends TransactionType("b010")
  case object ATOMIC extends TransactionType("b011")
  case object CACHE extends TransactionType("b100")
  case object VOLREL extends TransactionType("b101")

  def acquireTypes = Set(GET, PUT, HINT, ATOMIC, CACHE)
  def grantTypes = Set(GET, PUT, HINT, ATOMIC, CACHE, VOLREL)

  val acquireTypeWidth = 3
  val grantTypeWidth = 3
  val acquireHintWidth = 2

  implicit def ttype2UInt(x: TransactionType) = x.toUInt
  implicit def ttypeTuple2UInt[T](x: (TransactionType, T)) = (x._1.toUInt, x._2)
}

abstract class TileLinkBundle(implicit val p: Parameters) extends junctions.ParameterizedBundle()(p)
  with HasTileLinkParameters

abstract class TileLinkModule(implicit val p: Parameters) extends Module with HasTileLinkParameters {
  val io: HasTileLinkIO
}

trait HasTileLinkIO extends TileLinkBundle {
  val nClientPorts: Int
  val nManagerPorts: Int
  val clients  = Vec(nClientPorts, new TileLinkIO)
  val managers = Vec(nManagerPorts, new TileLinkIO).flip
}

