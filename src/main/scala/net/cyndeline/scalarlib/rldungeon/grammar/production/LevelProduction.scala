package net.cyndeline.scalarlib.rldungeon.grammar.production

import net.cyndeline.scalarlib.rldungeon.grammar.util.Morphism
import scalax.collection.GraphEdge.UnDiEdge
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Interface for classes that modify a graph. Used for injection.
 */
trait LevelProduction[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]] {
  def apply(level: L): Option[L]
  def getMatch(level: L): Option[Morphism[R]]
}
