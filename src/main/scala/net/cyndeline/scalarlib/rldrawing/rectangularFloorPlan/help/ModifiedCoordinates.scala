package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help

import net.cyndeline.scalarlib.rldrawing.common.RectangleCoordinates
import net.cyndeline.scalarlib.rldrawing.util.Point

/**
 * The start and stop coordinates of a rectangle that's been modified somehow. Wrapped in a class rather than a
 * tuple for readability.
 */
case class ModifiedCoordinates(start: Point, stop: Point) extends RectangleCoordinates {

}
