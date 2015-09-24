package net.cyndeline.scalarlib.rldungeon.dgs

import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Represents an aspect of a level.
 *
 * @constructor Creates a new parameter object.
 * @param name Name of the parameter. Mainly for debugging, not used in algorithms.
 * @param minValue The lowest value that this parameters aspect may assume and still be considered valid.
 * @param maxValue The highest value that this parameters aspect may assume and still be considered valid.
 * @param precision Used when comparing floating point values, since floating point values tend to be a bit imprecise.
 *                  When comparing to values A and B that are "equal", the actual fuzzy comparison will be true if
 *                  abs(A - B) is less than the precision.
 * @param estimator The object in charge of parsing a level into a value.
 * @param hasPriority True if this parameter has priority to override other non-priority parameters when rejecting
 *                    graphs based on their value, otherwise false.
 * @tparam R Room type used in the level.
 * @tparam C Corridor/connection type used in the level.
 */
final class Parameter[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]]
  (val name: String,
   minValue: Double,
   maxValue: Double,
   precision: Double,
   val estimator: ParameterEstimator[L, R, C],
   val hasPriority: Boolean) {

  require(maxValue >= minValue, "Parameter max value must be higher or equal to min value.")

  /**
   * @constructor Constructs a new parameter with default precision of 0.000001.
   * @param name Name of the parameter. Mainly for debugging, not used in algorithms.
   * @param minValue The lowest value that this parameters aspect may assume and still be considered valid.
   * @param maxValue The highest value that this parameters aspect may assume and still be considered valid.
   * @param estimator The object in charge of parsing a map into a value.
   * @param hasPriority True if this parameter has priority to override other non-priority parameters when rejecting
   *                    graphs based on their value, otherwise false.
   */
  def this(name: String, minValue: Double, maxValue: Double, estimator: ParameterEstimator[L, R, C], hasPriority: Boolean) =
    this(name, minValue, maxValue, 0.000001, estimator, hasPriority)

  /**
   * The value between max and min that the parameter strives to achieve.
   */
  def target: Double = ((maxValue - minValue) / 2) + minValue

  /**
   * Estimates the value of this parameter for a level.
   * @param level Level to estimate parameter value for.
   * @return Parameter value of the graph.
   */
  def estimate(level: L): Double = estimator.value(level)

  /**
   * Called to validate the final level.
   * @param level Level to validate.
   * @return True if the estimated parameter value for this level is within the min/max bounds, otherwise false.
   */
  def validate(level: L): Boolean = {
    val e = estimate(level)
    isInBounds(e)
  }

  /**
   * Compares two estimates related to the parameter and checks if the new estimate is closer
   * to the target or not.
   * @param oldEstimate Previous estimate of a graph by this parameters estimator.
   * @param newEstimate An estimate of a modified graph.
   * @return Accepted if the new estimate is closer to the target than before or in bounds.
   *         Indifferent if there's no difference in estimates. Rejected if the estimate is further
   *         from the target and out of bounds.
   */
  def compare(oldEstimate: Double, newEstimate: Double): ParameterResult = {
    val oldDifference = Math.abs(oldEstimate - target)
    val newDifference = Math.abs(newEstimate - target)

    if (newDifference < oldDifference)
      Accepted
    else if (Math.abs(newDifference - oldDifference) < precision)
      Indifferent
    else
      Rejected
  }

  private def isInBounds(v: Double): Boolean = v >= minValue && v <= maxValue

}
