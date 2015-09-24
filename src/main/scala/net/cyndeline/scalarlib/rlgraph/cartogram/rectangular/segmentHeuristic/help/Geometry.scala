package net.cyndeline.scalarlib.rlgraph.cartogram.rectangular.segmentHeuristic.help

import net.cyndeline.scalarlib.rlgraph.cartogram.rectangular.common.SegmentsOfRectangle
import net.cyndeline.scalarlib.rldrawing.util.Geom

/**
 * Common size methods.
 */
object Geometry {
  def area(s: SegmentsOfRectangle): Int = width(s) * height(s)
  def width(s: SegmentsOfRectangle): Int = Geom.width(s.left.value, s.right.value)
  def height(s: SegmentsOfRectangle): Int = Geom.height(s.top.value, s.bottom.value)
}
