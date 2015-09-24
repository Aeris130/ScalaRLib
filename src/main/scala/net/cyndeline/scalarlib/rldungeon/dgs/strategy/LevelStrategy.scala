package net.cyndeline.scalarlib.rldungeon.dgs.strategy

import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.grammar.Strategy

import scalax.collection.GraphEdge.UnDiEdge

/**
 * This is the main object responsible for generating a level using a set of strategies. If one strategy fails to
 * generate a level, no result is produced. Otherwise, the result from one strategy is passed onto the next.
 *
 * @param strategies The strategies that should be used to produce levels, in the order they should be applied to
 *                   the level.
 */
class LevelStrategy[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]](strategies: Vector[Strategy[L, R, C]]) extends Strategy[L, R, C] {

  /**
   * Produces a level.
   *
   * @param initialLevel The level that the productions initially will be applied to. This object may initially contain
   *                     whatever data the user wishes to use as starting point for the level, if any.
   * @return the finished level if one was possible to produce according to the strategies, otherwise None.
   */
  def apply(initialLevel: L): Option[L] = {
    val allStrategies = strategies.iterator
    var currentLevel = initialLevel

    while (allStrategies.hasNext) {
      val strategy = allStrategies.next()
      val result: Option[L] = strategy.apply(currentLevel)

      if (result.isEmpty)
        return None
      else
        currentLevel = result.get
    }

    Some(currentLevel)
  }

}
