package net.cyndeline.scalarlib.rldrawing

import net.cyndeline.rlcommon.util.{HeightConstraint, WidthConstraint}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.OrthogonalLayoutAlgorithm
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing.GridDrawing
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.{Compaction, CorridorProperties}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * The main object available to users when creating grid layouts from an orthogonal graph drawing.
 */
object OrthoGridArchitect {

  def buildDrawing[V <: WidthConstraint with HeightConstraint : TypeTag : ClassTag, E[X] <: UnDiEdge[X] with CorridorProperties : ({type l[M[_]] = TypeTag[M[V]]})#l : ({type l[M[_]] = ClassTag[M[V]]})#l]
  (graph: Graph[V, E]): GridDrawing[V, E[V]] = buildDrawingWithSettings(graph, new OrthoGridSettings())

  def buildDrawingWithSettings[V <: WidthConstraint with HeightConstraint : TypeTag : ClassTag, E[X] <: UnDiEdge[X] with CorridorProperties : ({type l[M[_]] = TypeTag[M[V]]})#l : ({type l[M[_]] = ClassTag[M[V]]})#l]
  (graph: Graph[V, E], settings: OrthoGridSettings): GridDrawing[V, E[V]] = {
    val random = new Random(12345) // Used to select a random target to compact towards
    val layoutAlgorithm = new OrthogonalLayoutAlgorithm[V, E]()
    val compaction = new Compaction[V, E](random, settings.wallIntersects)

    val initialLayout = layoutAlgorithm.orthogonalize(graph)
    val grid = compaction.compact(initialLayout)

    grid
  }
}
