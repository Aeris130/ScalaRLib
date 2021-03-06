package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Rectangle

/**
 * The start and stop coordinates of a rectangle that's been modified somehow. Wrapped in a class rather than a
 * tuple for readability.
 */
case class ModifiedCoordinates(start: Point, stop: Point) extends Rectangle {

}
