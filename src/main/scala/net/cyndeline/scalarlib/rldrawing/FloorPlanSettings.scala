package net.cyndeline.scalarlib.rldrawing

import net.cyndeline.rlgraph.drawings.planar.rectangular.OuterFaceSelection

import scalax.collection.GraphEdge.UnDiEdge

/**
 * User-specified settings for generating rectangular duals.
 */
//TODO Make immutable when the content has been finalized.
class FloorPlanSettings[V, E[X] <: UnDiEdge[X]] {
  private var outerFaceSelect: Option[OuterFaceSelection[V, E]] = None
  private var cartoErrorWeight: Double = 1.0
  private var aspectErrorWeight: Double = 1.0
  private var iterations = 10
  private var outer: Option[V] = None

  /** The maximum value of width/height and height/width that the segment heuristic should allow when optimizing the
    * initial layout. No area will have its size increased if doing so breaks this ratio. A higher ratio enables
    * better optimization in terms of target areas, but may result in disproportionate rectangles.
    */
  var maxAspectRatio: Double = 3

  /** This is a maximum aspect ratio that is used only for visual optimization of the final layout. Set it to something
    * that looks good, rather than something that gives a lot of leeway for area optimization.
    */
  var visualAspectRatio: Double = 2

  /** Vertices whose areas should not be optimized by size or aspect ratio. */
  var heuristicExceptions = Set[V]()

  /** The smallest size allowed for each rectangle in the drawing. Example: Setting this to 3 will make every
    * rectangle be of size 3x3 or higher. heuristicExceptions are not affected by this.
    */
  var minimumSideSize: Int = 1

  /** If set to true, causes the genetic optimization algorithm to be applied to the layout before returning
    * it. If false, only the heuristic is ran once.
    */
  var geneticOptimization = true

  def outerFaceSelection_=(s: OuterFaceSelection[V, E]): Unit = { outerFaceSelect = Some(s) }
  def outerFaceSelection: Option[OuterFaceSelection[V, E]] = outerFaceSelect

  def cartographicErrorWeight_=(w: Double): Unit = {
    require(w >= 0, "Weight must be >= 0")
    cartoErrorWeight = w
  }

  def aspectRatioErrorWeight_=(w: Double): Unit = {
    require(w >= 0, "Weight must be >= 0")
    aspectErrorWeight = w
  }

  /**
   * The cartographic error for each area in the layout will be multiplied by this amount.
   */
  def cartographicErrorWeight = cartoErrorWeight

  /**
   * The aspect ratio error for each area in the layout will be multiplied by this amount.
   */
  def aspectRatioErrorWeight = aspectErrorWeight

  /**
   * @return How many times the algorithm should run the evolutionary optimization algorithm. A higher iteration
   *         count can result in better layouts for large graphs, but requires more time to complete.
   */
  def optimizationIterations: Int = iterations

  def optimizationIterations_=(i: Int): Unit = {
    require (i >= 0, "Negative optimization iterations are not allowed.")
    iterations = i
  }

  /** The room that has been marked as the entry to the floor plan. Only set this if the entry room is guaranteed to
    * be placed on the outer edges of the floor plan, either by supplying an outer face-selection that selects a face
    * with the entry room, or by inputting a graph with few enough vertices that no area ends up being surrounded by
    * other areas on all 4 sides.
    */
  def entryRoom: Option[V] = outer
  def entryRoom_=(r: V): Unit = { outer = Some(r) }
}
