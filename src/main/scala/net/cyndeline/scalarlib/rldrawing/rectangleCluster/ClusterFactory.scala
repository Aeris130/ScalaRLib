package net.cyndeline.scalarlib.rldrawing.rectangleCluster

import net.cyndeline.scalarlib.rldrawing.util.RectangleArea

/**
  * Arranges a set of rectangular areas in a way that no area overlaps. Tries to pack the rectangles into a cluster.
  */
trait ClusterFactory {

  /**
    * @param rectangles Every rectangle to pack.
    * @param ratio The width to height ratio that the cluster should strive towards. 1 results in a rectangle, while
    *              a ratio greater than 1 results in a cluster that is wider than it is tall and vice versa. The
    *              factorys ability to achieve this ratio depends on the shape of the rectangles inputted.
    * @param edgeIntersect True if rectangles should be packed such that adjacent rectangles share their edge
    *                      coordinates, otherwise false.
    * @return The input vector of rectangles, given new coordinates so as to pack them into a cluster. A rectangle
    *         at a given index corresponds to the rectangle at the same index in the input vector. No rectangle will
    *         be resized or rotated to achieve a tighter packing or closer ratio.
    */
  def arrangeRectangles(rectangles: Vector[RectangleArea], ratio: Double, edgeIntersect: Boolean): Vector[RectangleArea]

}
