package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction

import net.cyndeline.rlcommon.util.{HeightConstraint, WidthConstraint}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.OrthogonalRepresentation
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction.AreaCompaction
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing.GridDrawing
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory.OrthogonalAreaFactory
import net.cyndeline.scalarlib.subcut.ProjectConfiguration

import scala.language.higherKinds
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Compacts an orthogonal drawing into an immutable grid drawing.
 *
 * @param random Used to determine a point on the map to compact areas toward.
 * @param intersect True if non-connected areas should be allowed to intersect at their borders, otherwise false.
 */
class Compaction[VType <: WidthConstraint with HeightConstraint, EType[X] <: UnDiEdge[X] with CorridorProperties]
  (random: Random, intersect: Boolean) {
  private val orthoAreaFactory = new OrthogonalAreaFactory(intersect)
  private implicit val bindingModule = ProjectConfiguration

  /**
   * Computes a compact grid drawing based on orthogonal vertex coordinates.
   *
   * @param drawing The orthogonal drawing to compute areas from.
   * @return A grid drawing with each vertex and edge being represented by a 2-dimensional area.
   */
  def compact(drawing: OrthogonalRepresentation[VType, EType]): GridDrawing[VType, EType[VType]] = {
    val areaRep = orthoAreaFactory.convertToAreas(drawing)
    val areaCompaction = new AreaCompaction[VType, EType[VType]](areaRep, random)

    areaCompaction.completeCompact()
    areaCompaction.produceCompactedDrawing
  }

}
