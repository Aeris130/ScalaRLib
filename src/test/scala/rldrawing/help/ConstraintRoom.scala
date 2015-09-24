package rldrawing.help

import net.cyndeline.scalarlib.rldrawing.common.{HeightConstraint, WidthConstraint}
import net.cyndeline.scalarlib.rlgraph.cartogram.rectangular.common.MapArea

/**
 * A room with width and height.
 */
class ConstraintRoom(w: Int, h: Int) extends HeightConstraint with WidthConstraint with MapArea {
  def elementWidth: Int = w
  def elementHeight: Int = h
  def targetArea: Int = elementWidth * elementHeight
}

object ConstraintRoom {
  def apply(w: Int, h: Int) = new ConstraintRoom(w, h)
}

case class ConstraintIdRoom(w: Int, h: Int, id: Int) extends HeightConstraint with WidthConstraint with MapArea {
  def elementWidth: Int = w
  def elementHeight: Int = h
  def targetArea: Int = elementWidth * elementHeight
}

