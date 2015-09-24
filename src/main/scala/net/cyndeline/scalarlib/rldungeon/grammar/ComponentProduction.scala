package net.cyndeline.scalarlib.rldungeon.grammar

import scalax.collection.GraphEdge.UnDiEdge
import net.cyndeline.scalarlib.rldungeon.grammar.util.Morphism
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Represents the right-hand side of a production rule, and modifies a level based on a morphism.
 */
trait ComponentProduction[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]] {

  /**
   * Performs a modification of a level.
   *
   * @param morphism Maps a set of vertices in a production pattern to the vertices in the levels sub graph.
   *                 It's up to the ComponentProductions implementation to know which vertices to use as keys here.
   * @param level The level that the sub graph appears in, in its entirety. Every room here is not guaranteed to
   *              appear in the morphism. This object is only supplied to allow ComponentProductions to perform
   *              level-spanning algorithms that doesn't rely on the content of the pattern, as well as giving them the
   *              level as it appears before modification.
   *              NOTE: Do not use this object to check if it is valid for a particular modification, put
   *              those checks in the negative condition inside the Production class.
   * @return a copy of the input level, modified by this production.
   */
  def apply(morphism: Morphism[R], level: L): L

}
