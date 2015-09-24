package net.cyndeline.scalarlib.rldungeon.grammar

import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.grammar.production.LevelProduction

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * A basic map-generating strategy that randomly selects individual productions without probabilities.
 *
 * @constructor Creates a new default strategy.
 * @param random Handles randomization of production selection.
 * @param productions Every production to apply.
 * @param averageDerivations The number of times a production will be selected at random and applied.
 * @param attempts If the graph reaches a state where it contains non-terminal vertices, with no further productions
 *                 that can be applied, the graph must be reset to its initial state and have the process start over.
 *                 This value controls the number of times these resets occur before the strategy gives up.
 * @param nonTerminal A set of graph vertices that are not allowed in the final graph. If one of these are present
 *                    when the 'averageDerivations amount is reached, the strategy will have to increase the
 *                    amount of times a productions to be applied.
 */
class DefaultStrategy[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]]
        (random: Random,
         productions: Vector[LevelProduction[L, R, C]],
         averageDerivations: Int,
         attempts: Int,
         nonTerminal: Set[R]) extends Strategy[L, R, C] {

  /**
   * Constructs a new DefaultStrategy with a new Random object.
   *
   * @param productions Every production to apply.
   * @param averageDerivations The number of times a production will be selected at random and applied.
   * @param nonTerminal A set of graph vertices that are not allowed in the final graph. If one of these are present
   *                    when the 'averageDerivations amount is reached, the strategy will have to increase the
   *                    amount of times a productions to be applied.
   * @param attempts If the graph reaches a state where it contains non-terminal vertices, with no further productions
   *                 that can be applied, the graph must be reset to its initial state and have the process start over.
   *                 This value controls the number of times these resets occur before the strategy gives up.
   */
  def this(productions: Vector[LevelProduction[L, R, C]],
           averageDerivations: Int,
           nonTerminal: Set[R],
           attempts: Int) = this(new Random(), productions, averageDerivations, attempts, nonTerminal)

  /**
   * Modifies a level and outputs the result.
   *
   * @param level The level that the productions initially will be applied to.
   * @return The finished level if one was possible to produce according to the strategy, otherwise None.
   *         The resulting level must have a single start room and a single goal, and be connected.
   */
  override def apply(level: L): Option[L] = {
    var attemptsLeft = attempts
    var currentLevel = level

    while (attemptsLeft > 0) {
      attemptsLeft -= 1
      var derivationsLeft = gaussianMultiplier(averageDerivations)
      var validProductionFound = false

      while (derivationsLeft > 0) {
        derivationsLeft -= 1
        val producedGraph = applyProduction(currentLevel)

        if (producedGraph.isDefined) {
          currentLevel = producedGraph.get
          validProductionFound = true
        } else {

          /* No valid production found, so no further derivations are possible. */
          derivationsLeft = 0
        }

        /* If the graph isn't valid at the time of the final derivation, but a valid production was found on the last
         * modification, it means there could be additional productions that hasn't been tried yet that still can make
         * the graph valid. Reset the derivation amount.
         *
         * If the graph is valid, no further attempts are needed.
         */
        if (derivationsLeft < 1) {
          val validStatus = graphIsValid(currentLevel.asGraph)

          if(!validStatus && validProductionFound)
            derivationsLeft = gaussianMultiplier(averageDerivations)
          else if (validStatus)
            return Option(currentLevel)

          // If the graph is not valid, and no valid production was found, the loop simply exists and starts another attempt
        }
      }

      // New attempt
    }

    None
  }

  /**
   * Selects random productions until one is found that can be applied to the submitted graph. Returns None if no
   * such production exists.
   */
  private def applyProduction(level: L): Option[L] = {
    val productionArray: ArrayBuffer[LevelProduction[L, R, C]] = ArrayBuffer(productions : _*)
    while(!productionArray.isEmpty) {
      val index = random.nextInt(productionArray.size)
      val resultingLevel = productionArray(index).apply(level)

      if (resultingLevel.isDefined)
        return resultingLevel
      else
        productionArray.remove(index)
    }

    None
  }

  /**
   * Checks if the graph contains non terminal vertices.
   */
  private def graphIsValid(graph: Graph[R, C]): Boolean = {
    val nodes = graph.nodes.iterator
    while(nodes.hasNext) {
      val n: R = nodes.next()
      if (nonTerminal.contains(n))
        return false
    }

    true
  }

  private def gaussianMultiplier(n: Int): Int = Math.abs(random.nextGaussian() * n).toInt
}
