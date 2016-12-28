package net.cyndeline.scalarlib.rldrawing.common

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle, Shape}

/**
  * A rectangular room.
  */
class RRoom(val id: Int, s: Point, w: Int, h: Int) extends Rectangle(s, w, h) with Shape[RRoom] with Room {

  override def +(x: Int, y: Int): RRoom = RRoom(id, super.+(x, y))
  override def -(x: Int, y: Int): RRoom = RRoom(id, super.-(x, y))
  override def *(x: Int, y: Int): RRoom = RRoom(id, super.*(x, y))

  override def toString: String = s"Room[$id]:[$start, width $width, height $height]"
  override val hashCode: Int = {
    var result = 41
    result = result * 41 + id.##
    result = result * 41 + s.##
    result = result * 41 + w.##
    result = result * 41 + h.##
    result
  }
  override def equals(other: Any): Boolean = other match {
    case rr: RRoom => id == rr.id && start == rr.start && width == rr.width && height == rr.height
    case _ => false
  }

}

object RRoom {
  def apply(id: Int, shape: Rectangle) = new RRoom(id, shape.start, shape.width, shape.height)
}
