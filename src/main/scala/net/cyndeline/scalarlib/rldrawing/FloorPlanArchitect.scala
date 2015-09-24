package net.cyndeline.scalarlib.rldrawing

import net.cyndeline.rlcommon.util.Geom
import net.cyndeline.rlgraph.cartogram.rectangular.common.MapArea
import net.cyndeline.rlgraph.cartogram.rectangular.evolution.EvolutionaryOptimization
import net.cyndeline.rlgraph.cartogram.rectangular.evolution.metrics.{AspectRatio, CartographicError}
import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.SegmentHeuristic
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.{RAlgorithmSettings, RectangularDualAlgorithm}
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.FloorPlan
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help.{ChocoMinSize, MinimumSizeIncrease, SegmentReducer}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * This is the main factory object used to create floor plans. Only access the individual classes when sub-algorithms
 * needs to be substituted for others.
 *
 * The algorithm performs the following steps, given an input graph G:
 *
 *  1. The graph is converted to a regular edge labeling
 *  1. The labeling is optimized using an evolutionary heuristic
 *  1. The smallest increase needed to scale the optimized layout such that every room meets its target area is
 *  computed using a heuristic that prioritizes keeping the aspect ratio even.
 *  1. Room that exceed its target area are reduced in size from one side, such that adjacencies are kept intact.
 *
 * Options regarding the final drawing may be specified, but default settings can also be used.
 */
object FloorPlanArchitect {

  def buildDrawing[V <: MapArea : TypeTag : ClassTag, E[X] <: UnDiEdge[X] : ({type l[M[_]] = TypeTag[M[V]]})#l]
  (graph: Graph[V, E]): FloorPlan[V, E] = buildDrawingWithSettings(graph, new FloorPlanSettings[V, E]())

  def buildDrawingWithSettings[V <: MapArea : TypeTag : ClassTag, E[X] <: UnDiEdge[X] : ({type l[M[_]] = TypeTag[M[V]]})#l]
    (graph: Graph[V, E], settings: FloorPlanSettings[V, E]): FloorPlan[V, E] = {
    require(settings.minimumSideSize >= 3, "The minimum size of each room side must be 3 or more (2 walls and 1 floor tile).")
    require(settings.maxAspectRatio >= 1, "Aspect ratios must be 1 or higher.")
    val minSizeAddition: MinimumSizeIncrease = new ChocoMinSize()
    val algorithmSettings = if (settings.outerFaceSelection.isDefined)
      RAlgorithmSettings[V, E]().withOuterFaceSelect(settings.outerFaceSelection.get)
    else
      RAlgorithmSettings[V, E]()

    /* Step 1: Compute the dual. If the graph has 3 or less vertices, use the regular algorithm. Otherwise compute
     * a regular edge labeling optimization.
     */
    val segmentHeuristic = new SegmentHeuristic[V, E](settings.maxAspectRatio)
    val rAlgorithm = RectangularDualAlgorithm.regularAlgorithmWithSettings(algorithmSettings)
    val layout = if (graph.nodes.size < 4) {
      rAlgorithm.computeLayout(graph)

    } else if (settings.geneticOptimization) {
      val algorithmAndRel = RectangularDualAlgorithm.edgeLabelAlgorithmWithSettings(graph, algorithmSettings)
      val algorithm = algorithmAndRel._1
      val rEdgeLabeling = algorithmAndRel._2
      val cartographicErrorScore = new CartographicError(settings.cartographicErrorWeight)
      val aspectRatioScore = new AspectRatio(settings.aspectRatioErrorWeight, 22)
      val evoOptimizer = new EvolutionaryOptimization(settings.optimizationIterations, Vector(cartographicErrorScore, aspectRatioScore), segmentHeuristic, settings.heuristicExceptions)
      evoOptimizer.optimizeLayout(rEdgeLabeling, algorithm)

    } else {
      segmentHeuristic.applyToLayoutWithExceptions(rAlgorithm.computeLayout(graph), settings.heuristicExceptions)
    }

    /* Step 2: Increase every rectangle in size such that every room meets its target area.
     *
     * Note !!! This algorithm is also responsible for ensuring that every adjacency constraint has 1 extra tile
     * margin, to prevent the player from moving diagonally between walls at an intersection.
     */
    val minSizeLayout = minSizeAddition.increaseToMinimumWithExceptions(layout, settings.heuristicExceptions, settings.minimumSideSize)

    /* Step 3: Adjust the individual sides of the final layout. At this point it is no longer a valid rectangular dual. */
    val plan = FloorPlan(minSizeLayout)
    val reducer = new SegmentReducer(settings.visualAspectRatio, settings.visualAspectRatio, settings.minimumSideSize, settings.minimumSideSize)
    val reduced = if (settings.entryRoom.isDefined) {
      verifyEntryRoomOnOutsideOfPlan(plan, settings.entryRoom.get)
      reducer.reduceFloorPlanWithEntry(plan, settings.entryRoom.get)
    } else {
      reducer.reduceFloorPlan(plan)
    }

    verifyRoomSize(reduced)
    reduced
  }

  /* Call this while the floor plan is still a valid rectangular dual. */
  private def verifyEntryRoomOnOutsideOfPlan[V <: MapArea, E[X] <: UnDiEdge[X]](floorPlan: FloorPlan[V, E], entry: V) {
    val entryArea = floorPlan.roomAreas.find(area => area.isRoom && area.originalRoom == entry)
      .getOrElse(throw new IllegalArgumentException("Could not find a floor plan area matching the supplied entry room " + entry + "."))

    if (entryArea.start.x != floorPlan.minX && entryArea.start.y != floorPlan.minY && entryArea.stop.x != floorPlan.maxX && entryArea.stop.y != floorPlan.maxY) {
      throw new IllegalArgumentException("The entry room " + entry + " could not be found on the outside border of the floor plan, " +
        "where it must be in order for the user to enter the layout from outside. If the placement of the starting room is " +
        "irrelevant, the room should not be set in the settings object.")
    }
  }

  private def verifyRoomSize[V <: MapArea, E[X] <: UnDiEdge[X]](floorPlan: FloorPlan[V, E]) {
    val invalid = floorPlan.roomAreas.find(r => r.isRoom && Geom.area(r) < r.originalRoom.targetArea)
    if (invalid.isDefined)
      throw new Error("The final floor plan contained at least one room whose area is less than its target: " + invalid.get)
  }
}
