package rldrawing.help

import net.cyndeline.scalarlib.rldrawing.MapLayout
import net.cyndeline.scalarlib.rldrawing.common.RRoom

/**
  * Validates map layouts.
  */
object MapLayoutValidation {

  def validate(layout: MapLayout[RRoom]): Unit = {
    for (i <- layout.rooms.indices; j <- layout.rooms.indices if i != j) {
      val a = layout.rooms(i)
      val b = layout.rooms(j)
      val intersection = a.intersection(b)

      if (intersection.nonEmpty) {
        if (a.width > 1 && b.width > 1)
          assert(intersection.get.width == 1 || intersection.get.height == 1)
        else
          throw new Error("Single-coordinate x overlap between single-coordinate rooms")

        if (a.height > 1 && b.height > 1)
          assert(intersection.get.width == 1 || intersection.get.height == 1)
        else
          throw new Error("Single-coordinate y overlap between single-coordinate rooms")
      }
    }
  }

}
