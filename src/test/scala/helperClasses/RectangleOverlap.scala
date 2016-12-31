package helperClasses

import net.cyndeline.rlcommon.math.geom.Rectangle

/**
  * Checks a rectangle set for overlap beyond borders.
  */
object RectangleOverlap {

  def overlaps(rs: Vector[Rectangle]): Boolean = {
    for (i <- rs.indices; j <- rs.indices if i != j) {
      val intersection = rs(i).intersection(rs(j))
      if (intersection.isDefined && intersection.get.width > 1 && intersection.get.height > 1)
        return true
    }

    false
  }

}
