package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.OuterEdge
import net.cyndeline.scalarlib.rldungeon.grammar.Strategy
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * A strategy that always returns its input.
 *
 * @constructor Creates a new dummy strategy.
 */
class DummyStrategy[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]]
  extends Strategy[L, R, C]  {

  def apply(initialLevel: L): Option[L] = Option(initialLevel)
}
