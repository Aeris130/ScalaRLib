package net.cyndeline.scalarlib.rldrawing

import net.cyndeline.rlcommon.math.geom.Point

/**
  * Stores one or multiple coordinate intervals for two connected areas. Every coordinate along each interval can be
  * used to move from one area to the other, only the coordinates that take part of the opening are included here.
  *
  * @param openings A list of openings, where each opening is a tuple of points representing a range of coordinates
  *                 shared by both areas.
  */
case class Opening(a1: Int, a2: Int, openings: Vector[(Point, Point)]) {

}
