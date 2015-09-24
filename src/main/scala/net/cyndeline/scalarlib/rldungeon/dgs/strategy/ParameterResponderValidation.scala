package net.cyndeline.scalarlib.rldungeon.dgs.strategy

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.OuterEdge
import net.cyndeline.scalarlib.rldungeon.dgs.Parameter
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Decides if a change to a level graph is considered valid by evaluating parameter responses to the change.
 *
 * When implementing this trait, make sure that a pair of subsequent modifications (A, and B which undoes A by
 * reverting the map to the state before A was applied) can't both be considered valid during the same level
 * generation. Otherwise the algorithm could be sent into an infinite loop by having two productions push the
 * map back and forth between the states A and B.
 */
trait ParameterResponderValidation[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]] {

  /**
   * Decides if a level modification should be kept or discarded based on how parameters respond to the change.
   *
   * @param acceptingParameters Every parameter that accepts the change (if any).
   * @param rejectingParameters Every parameter that rejects the change (if any).
   * @return True if the map modification should be kept, otherwise false.
   */
  def levelModificationValidates(acceptingParameters: Set[Parameter[L, R, C]],
                                 rejectingParameters: Set[Parameter[L, R, C]]): Boolean

}
