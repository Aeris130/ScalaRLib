package net.cyndeline.scalarlib.rldrawing.util

/**
 * A pair of x/y coordinates.
 *
 * @constructor Constructs a new point representing a coordinate on the positive side of the cartesian grid.
 * @param x The x dimension on the grid. Must be 0 or higher.
 * @param y The y dimension on the grid. Must be 0 or higher.
 */
case class Point(x: Int, y: Int) {
  if (x < 0 || y < 0) throw new Error("The coordinates for point (" + x + "," + y + ") must be positive")

  /**
   * @param t A tuple containing the x and y coordinate the create a new point from.
   * @return A point object using t as x/y coordinates.
   */
  def this(t: (Int, Int)) = this(t._1, t._2)

  def asTuple: (Int, Int) = (x, y)

  def modify(amount: Int): Point = Point(x + amount, y + amount)
  def modifyX(amount: Int): Point = Point(x + amount, y)
  def modifyY(amount: Int): Point = Point(x, y + amount)

  override def equals(other: Any): Boolean = other match {
    case p: Point => x == p.x && y == p.y
    case _ => false
  }

  override def hashCode: Int = x ^ (y << 32)

}
