package net.cyndeline.scalarlib.rldungeon.grammar.production

import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.grammar.util.Morphism

import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * Interface for classes that modify a graph. Used for injection.
 */
trait LevelProduction[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV] {
  def apply(level: L): Option[L]
  def getMatch(level: L): Option[Morphism[R, PV]]
}
