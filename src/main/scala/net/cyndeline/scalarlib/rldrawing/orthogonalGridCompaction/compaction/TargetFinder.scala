package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.AdjustableRectangle

/**
 * Computes the target coordinate that every area should be compacted against.
 * Separated into its own class to make testing easier.
 */
trait TargetFinder {

  /**
   * Computes a single point as compaction target.
 *
   * @param area A non-corridor segment area in the drawing (i.e a room or a bend).
   * @return The compaction target coordinate.
   */
  def findTarget(area: AdjustableRectangle): Point
}
