package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help

import solver.{ResolutionPolicy, Solver}
import scalax.collection.GraphEdge.UnDiEdge
import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import net.cyndeline.scalarlib.rlgraph.cartogram.rectangular.common._
import solver.variables.{VariableFactory => VF, IntVar}
import net.cyndeline.scalarlib.rlgraph.cartogram.rectangular.common.Constraint
import solver.constraints.{ IntConstraintFactory => ICF }

/**
 * Formulates the task of increasing the side of every rectangle in a rectangular dual to a minimum size as an
 * integer constraint problem, and solves it using the Choco solver.
 */
class ChocoMinSize extends MinimumSizeIncrease {

  def increaseToMinimum[V, E[X] <: UnDiEdge[X]](layout: RectangularLayout[V, E], min: Int): RectangularLayout[V, E]
    = increaseToMinimumWithExceptions(layout, Set(), min)

  def increaseToMinimumWithExceptions[V, E[X] <: UnDiEdge[X]](layout: RectangularLayout[V, E], exceptions: Set[V], min: Int): RectangularLayout[V, E] = {
    val minimumSize = min - 1
    val solverX = new Solver("Increasing the width of every area in a rectangular dual to minimum size")
    val solverY = new Solver("Increasing the height of every area in a rectangular dual to minimum size")
    val segmentMap = new SegmentMap(layout)
    val maxXBound = layout.maxX + (layout.allAreas.size * minimumSize * 2)
    val maxYBound = layout.maxY + (layout.allAreas.size * minimumSize * 2)
    val verticalVariables: Array[IntVar] = VF.boundedArray("VSeg", segmentMap.verticalSegments.size, 0, maxXBound, solverX)
    val horizontalVariables: Array[IntVar] = VF.boundedArray("HSeg", segmentMap.horizontalSegments.size, 0, maxYBound, solverY)

    val verticalAdjacencies = adjacencyConstraints(layout, segmentMap, X_Axis)
    val horizontalAdjacencies = adjacencyConstraints(layout, segmentMap, Y_Axis)
    addAdjacencyConstraints(solverX, verticalVariables, verticalAdjacencies)
    addAdjacencyConstraints(solverY, horizontalVariables, horizontalAdjacencies)

    addMinSizeConstraints(X_Axis, solverX, layout, segmentMap, exceptions, verticalVariables, minimumSize)
    addMinSizeConstraints(Y_Axis, solverY, layout, segmentMap, exceptions, horizontalVariables, minimumSize)

    val objectiveX = VF.bounded("Sum of x segment coordinates", 0, VF.MAX_INT_BOUND, solverX)
    val objectiveY = VF.bounded("Sum of y segment coordinates", 0, VF.MAX_INT_BOUND, solverY)
    solverX.post(ICF.sum(verticalVariables, objectiveX))
    solverY.post(ICF.sum(horizontalVariables, objectiveY))

    solverX.findOptimalSolution(ResolutionPolicy.MINIMIZE, objectiveX)
    solverY.findOptimalSolution(ResolutionPolicy.MINIMIZE, objectiveY)

    for (i <- 0 until verticalVariables.size)
      segmentMap.verticalSegments(i).value = verticalVariables(i).getValue

    for (i <- 0 until horizontalVariables.size)
      segmentMap.horizontalSegments(i).value = horizontalVariables(i).getValue

    segmentMap.constructNewDrawingFromSegments
  }



  /**
   * Doubles as planarity constraints.
   */
  private def addMinSizeConstraints[V, E[X] <: UnDiEdge[X]](axis: Axis,
                                                            solver: Solver,
                                                            layout: RectangularLayout[V, E],
                                                            segmentMap: SegmentMap[V, E],
                                                            exceptions: Set[V],
                                                            variables: Array[IntVar],
                                                            minSize: Int) {
    val rToV = layout.rectangles.map(_.swap)
    val rectangles = layout.allAreas

    for (r <- rectangles) {
      val s = segmentMap.segmentOfArea(r)

      if (!r.isVertex || (r.isVertex && !exceptions.contains(rToV(r)))) {
        axis match {
          case X_Axis => {
            val lower = s.left
            val upper = s.right
            val minWidth = Math.max(upper.value - lower.value, minSize)
            solver.post(ICF.arithm(variables(upper.variableIndex), "-", variables(lower.variableIndex), ">=", minWidth))
          }
          case Y_Axis => {
            val lower = s.top
            val upper = s.bottom
            val minHeight = Math.max(upper.value - lower.value, minSize)
            solver.post(ICF.arithm(variables(upper.variableIndex), "-", variables(lower.variableIndex), ">=", minHeight))
          }
        }
      } else {
        axis match {
          case X_Axis => {
            val lower = s.left
            val upper = s.right
            solver.post(ICF.arithm(variables(upper.variableIndex), ">", variables(lower.variableIndex)))
          }
          case Y_Axis => {
            val lower = s.top
            val upper = s.bottom
            solver.post(ICF.arithm(variables(upper.variableIndex), ">", variables(lower.variableIndex)))
          }
        }
      }
    }
  }

  /**
   * Normally it would be enough to have the lower segment be less than the upper, but since we're using these drawings
   * to compute maps, it's not enough to simply be 1 coordinate apart, as that allows the player to move diagonally
   * between the walls. The lower segment must be 1 extra coordinate away from the upper.
   */
  private def addAdjacencyConstraints(solver: Solver, variables: Array[IntVar], constraints: Vector[Constraint]) {
    for (c <- constraints) {
      val lower = if (c.relation == LessThan) c.other.variableIndex else c.segment.variableIndex
      val upper = if (c.segment.variableIndex == lower) c.other.variableIndex else c.segment.variableIndex
      solver.post(ICF.arithm(variables(lower), "<", variables(upper), "-", 1))
    }
  }

  /**
   * Every rectangle in the original layout must remain adjacent to its neighbors that doesn't share one of its two
   * edges along the axis being computed. See the AdjacencyConstraints class for more information.
   */
  private def adjacencyConstraints[V, E[X] <: UnDiEdge[X]](layout: RectangularLayout[V, E], segments: SegmentMap[V, E], axis: Axis): Vector[Constraint] = {
    (for {
      rectangle <- layout.allAreas
      if layout.adjacentAreas contains rectangle
      neighbors = layout.adjacentAreas(rectangle)
      rectSegments = segments.segmentOfArea(rectangle)
      neighborSegments = neighbors.map(segments.segmentOfArea)
      newConstraints = new AdjacencyConstraints(rectSegments, neighborSegments, axis).constraints
    } yield newConstraints).flatten
  }
}
