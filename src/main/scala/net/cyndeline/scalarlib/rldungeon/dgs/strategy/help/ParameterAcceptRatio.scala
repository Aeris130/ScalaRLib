package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.OuterEdge
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.ParameterResponderValidation
import net.cyndeline.scalarlib.rldungeon.dgs.Parameter
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Validates a set of parameters if the accepting amount exceeds the rejecting amount by a specified ratio.
 *
 * @constructor Creates a new accept ratio validator instance.
 * @param acceptLimit A value > 0, representing the percentage of accepting parameters that must exceed
 *                    the rejecting parameters when deciding to keep a graph modification or not. Example: A value of
 *                    0.2 means that the number of accepting parameters must exceed the rejecting ones by 20%. If
 *                    5 parameters reject a change, 6 must accept it. The reason the value has to be non-zero is
 *                    to prevent two sets of parameters from causing the algorithm to loop forever by accepting
 *                    and rejecting two productions back and forth. By having the amount of accepting parameters
 *                    be higher than the rejecting ones, the accept-rate for the level will always go up for
 *                    each accepted production.
 */
class ParameterAcceptRatio[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]]
  (acceptLimit: Double)
  extends ParameterResponderValidation[L, R, C] {

  if (acceptLimit <= 0)
    throw new IllegalArgumentException("Accept limit must be higher than 0.")

  private val acceptValue: Double = 1 + acceptLimit

  /**
   * Decides if a level modification should be kept or discarded based on how parameters respond to the change.
   *
   * @param acceptingParameters Every parameter that accepts the change.
   * @param rejectingParameters Every parameter that rejects the change.
   * @return True if the map modification should be kept, otherwise false.
   */
  def levelModificationValidates(acceptingParameters: Set[Parameter[L, R, C]], rejectingParameters: Set[Parameter[L, R, C]]): Boolean = {
    acceptingParameters.size > 0 && acceptingParameters.size >= (rejectingParameters.size * acceptValue)
  }
}
