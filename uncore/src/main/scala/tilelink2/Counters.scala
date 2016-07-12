package uncore.tilelink2

import Chisel._
import uncore.util.TwoWayCounter

class BeatCounterStatus extends Bundle {
  val count = UInt()
  val done = Bool()
}

class TwoWayBeatCounterStatus extends Bundle {
  val pending = Bool()
  val up = new BeatCounterStatus()
  val down = new BeatCounterStatus()
}

class TileLinkBeatCounter(channel: TileLinkChannel) {
  val maxCount = channel.tlMaxTransferSize / channel.tlPhysicalDataWidth
  val maxCountWidth = log2Up(maxCount)
  val countWidth = Log2(channel.size()) - UInt(log2Up(channel.tlPhysicalDataWidth))
  val count = UInt(1) << countWidth

  val value = Reg(init=UInt(0, maxCountWidth))

  def inc: Bool = {
    val wrap = Wire(Bool())
    when (count > UInt(1)) {
      value := value + UInt(1)
      wrap := value === count - UInt(1)
      when (wrap) { value := UInt(0) }
    } .otherwise {
      wrap := Bool(true)
    }
    wrap
  }
}

/** Returns the current count on this channel and when a message is done
  * @param inc increment the counter (usually .valid or .fire())
  * @param channel the actual channel data
  */
object TileLinkBeatCounter {
  def apply(cond: Bool, c: TileLinkChannel): BeatCounterStatus = {
    val status = Wire(new BeatCounterStatus) 
    val cnt = new TileLinkBeatCounter(c)
    val wrap = Wire(Bool())
    when (cond) { wrap := cnt.inc }
    status.count := cnt.value
    status.done := cond && wrap
    status
  }

  def apply(c: DecoupledIO[TileLinkChannel]): BeatCounterStatus = apply(c.fire(), c.bits)
}


/** Provides counters on two channels, as well a meta-counter that tracks how many
  * messages have been sent over the up channel but not yet responded to over the down channel
  *
  * @param up outgoing channel
  * @param down incoming channel
  * @param max max number of outstanding ups with no down
  * @param track whether up's message should be tracked
  * @return a tuple containing whether there are outstanding messages, up's count,
  *         up's done, down's count, down's done
  */
object TwoWayBeatCounter {
  def apply[T <: TileLinkChannel, S <: TileLinkChannel](
      up: DecoupledIO[T],
      down: DecoupledIO[S],
      max: Int = 1,
      trackUp: T => Bool = (t: T) => Bool(true),
      trackDown: S => Bool = (s: S) => Bool(true)): TwoWayBeatCounterStatus = {
    val status = Wire(new TwoWayBeatCounterStatus) 
    val upcnt = TileLinkBeatCounter(up.fire() && trackUp(up.bits), up.bits)
    val dncnt = TileLinkBeatCounter(down.fire() && trackDown(down.bits), down.bits)
    val cnt = TwoWayCounter(upcnt.done, dncnt.done, max)
    status.pending := cnt > UInt(0)
    status.up := upcnt
    status.down := dncnt
    status
  }
}
