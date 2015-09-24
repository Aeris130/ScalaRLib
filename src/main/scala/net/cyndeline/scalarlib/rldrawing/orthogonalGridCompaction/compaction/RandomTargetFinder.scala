package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction

import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.RectangularArea
import scala.util.Random
import net.cyndeline.scalarlib.rldrawing.util.Point

/**
 * Computes a random target coordinate.
 *
 * @constructor Constructs a new target finder using a random object to select among possible targets.
 * @param random Selects random integer coordinates from a range of values.
 */
class RandomTargetFinder(random: Random) extends TargetFinder {

  /**
   * Computes a single point as compaction target.
   * @param area The area that the target point must lie within.
   * @return The compaction target coordinate.
   */
  def findTarget(area: RectangularArea): Point = {
    val highestX = area.stop.x
    val highestY = area.stop.y
    val lowestX = area.start.x
    val lowestY = area.start.y
    val x = random.nextInt(highestX - lowestX + 1) + lowestX
    val y = random.nextInt(highestY - lowestY + 1) + lowestY
    Point(x, y)
  }
}
