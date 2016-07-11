package uncore.devices

import Chisel._
import uncore.util.Rom
import uncore.tilelink2._
import cde.Parameters
import ProtocolTransactionTypes._

class ROMSlave2(contents: Seq[Byte])(implicit p: Parameters) extends TileLinkSlaveDevice {
  val acq = Queue(io.clients.head.acquire, 1)
  assert(!acq.valid || acq.bits.isType(GET), "unsupported ROMSlave operation")

  val rom = Rom(tlPhysicalDataWidth, contents)

  Grant.fromAcquire(
    gnt = io.clients.head.grant,
    acq = acq,
    data = load(rom, acq.bits))
}
