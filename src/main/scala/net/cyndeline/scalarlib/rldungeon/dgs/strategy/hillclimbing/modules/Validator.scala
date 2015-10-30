package net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.modules

import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.dgs.{Accepted, Parameter, Rejected}

import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * Used for injection.
 */
trait ValidatorI {
  def validateModifiedGraph[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X]]
    (parameters: Set[Parameter[L, R, C]],
    previousEstimates: Map[Parameter[L, R, C], Double],
    newLevel: L): (Set[Parameter[L, R, C]], Set[Parameter[L, R, C]], Map[Parameter[L, R, C], Double])
}
/**
 * Checks which parameters considers a map modification closer to their target.
 *
 * @constructor Constructs a new validator.
 */
class Validator extends ValidatorI {

  /**
   * Separates a list of parameters into two sets of accepting and rejecting parameters (indifferent ones
   * are discarded).
   *
   * @param parameters Parameters to evaluate a level.
   * @param previousEstimates Values that the parameters estimated for the previous level.
   * @param newLevel A modified version of the previous level.
   * @return Two sets of parameters (accepting and rejecting) as well as every parameter mapped to its estimated value
   *         of the input level.
   */
  def validateModifiedGraph[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X]]
  (parameters: Set[Parameter[L, R, C]],
   previousEstimates: Map[Parameter[L, R, C], Double],
   newLevel: L): (Set[Parameter[L, R, C]], Set[Parameter[L, R, C]], Map[Parameter[L, R, C], Double]) = {

    var accepts = Set[Parameter[L, R, C]]()
    var rejects = Set[Parameter[L, R, C]]()
    var estimations = previousEstimates.empty
    val params = parameters.iterator
    while (params.hasNext) {
      val p = params.next()
      val newEstimate = p.estimator.value(newLevel)
      estimations += (p -> newEstimate)
      p.compare(previousEstimates(p), newEstimate) match {
        case Accepted => accepts += p
        case Rejected => rejects += p
        case _ =>
      }
    }

    (accepts, rejects, estimations)
  }

}
