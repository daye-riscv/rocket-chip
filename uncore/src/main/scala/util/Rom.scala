// See LICENSE for license details.

package uncore.util

import Chisel._

object Rom {
  def apply(width: Int, contents: Seq[Byte]): Vec[UInt] = {
    require(width >= 8 && width % 8 == 0)
    val byteWidth = width / 8
    val rows = (contents.size + byteWidth - 1)/byteWidth
    Vec.tabulate(rows) { i =>
      val slice = contents.slice(i*byteWidth, (i+1)*byteWidth)
      UInt(slice.foldRight(BigInt(0)) { case (x,y) => (y << 8) + (x.toInt & 0xFF) }, byteWidth*8)
    }
  }
}
