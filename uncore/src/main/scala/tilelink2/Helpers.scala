package uncore.tilelink2

import Chisel._
import cde.Parameters
import ProtocolTransactionTypes._

object Helpers {

  def wmask(dataWidth: Int, full_addr: UInt, size: UInt): UInt = {
    val subBeatAddrWidth = log2Up(dataWidth)
    val sub_addr = full_addr(subBeatAddrWidth-1,0)
    val mask = (UInt(1) << size) - UInt(1)
    val partial = mask << sub_addr
    val full = ~UInt(0, dataWidth/8)
    Mux(size >= UInt(log2Up(dataWidth)), full, partial) 
  }
}

/** Get data from the outer memory hierarchy
  *
  * The client can hint whether he block containing this beat should be 
  * allocated in the intervening levels of the hierarchy.
  *
  * @param source client's transaction id
  * @param full_addr address of the data to read
  * @param size will get 2**size bytes of data
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object Get {
  def apply(
        source: UInt,
        full_addr: UInt,
        size: UInt,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    val out = Wire(new Acquire)
    out.a_type := GET
    out.source := source
    out.addr := full_addr >> UInt(out.tlSubBeatAddrWidth)
    out.wmask := Helpers.wmask(out.tlPhysicalDataWidth, full_addr, size)
    out.sz := size
    out.union := alloc.toUInt
    out
  }
}

/** Prefetch a data block into the next-outermost level of the memory hierarchy.
  *
  * @param source client's transaction id
  * @param full_addr address of the data block
  * @param size will get 2**size bytes of data
  * @param rw hint whether the block should be allocated with write permissions
  */
object Hint {
  def apply(
        source: UInt,
        full_addr: UInt,
        size: UInt,
        rw: Bool)
      (implicit p: Parameters): Acquire = {
    val out = Wire(new Acquire)
    out.a_type := HINT
    out.source := source
    out.addr := full_addr >> UInt(out.tlSubBeatAddrWidth)
    out.wmask := Helpers.wmask(out.tlPhysicalDataWidth, full_addr, size)
    out.sz := size
    out.union := HintPolicySpecific(should = Bool(true), write = rw)
    out
  }
}

/** Put a single beat of data into the outer memory hierarchy
  *
  * The block will be allocated in the next-outermost level of the hierarchy.
  *
  * @param source client's transaction id
  * @param full_addr address of the cache block
  * @param data data being written to outer memory
  * @param wmask per-byte write mask for this beat
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object Put {
  def apply(
        source: UInt,
        full_addr: UInt,
        size: UInt,
        data: UInt,
        wmask: Option[UInt]= None,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    val out = Wire(new Acquire)
    out.a_type := PUT
    out.source := source
    out.addr := full_addr >> UInt(out.tlSubBeatAddrWidth)
    out.data := data
    out.wmask := wmask.getOrElse(Helpers.wmask(out.tlPhysicalDataWidth, full_addr, size))
    out.union := alloc
    out
  }

  /*
  TODO hide complexity of generation addrs for multiple beats
  def makePut(
        acq: DecoupledIO[Acquire],
        source: UInt,
        full_addr: UInt,
        data: UInt,
        wmask: Option[UInt]= None,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    channel.bits = apply(source, full_addr, data, wmask, alloc)
    val status = TileLinkBeatCounter(acq.fire(), acq.bits)
  */
}

/** Perform an atomic memory operation in the next-outermost level of the memory hierarchy
  *
  * @param client_xact_id client's transaction id
  * @param full_addr address of the atomic op
  * @param amo_opcode {swap, add, xor, and, min, max, minu, maxu} from [[uncore.MemoryOpConstants]]
  * @param size 2 or 3 (32b or 64b)
  * @param data source operand data
  */
object Atomic {
  import AtomicOpCodes._
  def apply(
        source: UInt,
        full_addr: UInt,
        opcode: UInt,
        size: UInt,
        data: UInt)
      (implicit p: Parameters): Acquire = {
    val out = Wire(new Acquire)
    out.a_type := ATOMIC
    out.source := source
    out.addr := full_addr >> UInt(out.tlSubBeatAddrWidth)
    out.data := data
    out.wmask := Helpers.wmask(out.tlPhysicalDataWidth, full_addr, size)
    out.union := opcode
    out
  }
}

