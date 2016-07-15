package net.cyndeline.scalarlib.rldungeon.grammar.production

import net.cyndeline.rlcommon.util.{ProbabilityCollection, RandomCollection}
import net.cyndeline.rlgraph.subgraph.isomorphism.proofProcess.VF2IMatcher
import net.cyndeline.rlgraph.subgraph.isomorphism.IsomorphicMapping
import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction
import net.cyndeline.scalarlib.rldungeon.grammar.util.{GraphMatcher, MorphismFactory}
import scala.language.higherKinds
import scala.util.Random
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

/**
 * Specifies a single left-hand side of a graph grammar, and multiple right-hand side ComponentProductions.
 * Allows random selection with probabilities between each ComponentProduction.
 *
 * @param pattern A graph whose vertices and edges matches the sought-after level topology to apply the production to.
 *                Edges will only be taken into account in the manner they connect vertices, not by checking
 *                edge-specific data. Must be a non-empty graph.
 * @param compProductions A list of all ComponentProductions that this multi production may apply, paired with
 *                        a value indicating the probability of that production to be selected. The probability
 *                        of an individual element is its value divided by the sum of all values.
 * @param random Used to select ComponentProductions randomly.
 * @param isomorphismMapper Examines every supplied levels graph for sub-structures that matches the pattern, and
 *                          returns one of them.
 * @param morphismFactory Factory responsible for building morphisms based on isomorphic mappings.
 * @param probabilitySelector Object responsible for randomly selecting which production to use.
 */
final class MultiProduction[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]] private
                                                             (pattern: Graph[PV, PE],
                                                              compProductions: Vector[(ComponentProduction[L, R, C, PV], Int)],
                                                              random: Random,
                                                              isomorphismMapper: IsomorphicMapping[R, C, PV, PE],
                                                              morphismFactory: MorphismFactory,
                                                              probabilitySelector: RandomCollection[ComponentProduction[L, R, C, PV]]
                                                              ) extends Production[L, R, C, PV, PE](pattern, random, isomorphismMapper, morphismFactory) {
  val productionProbabilitySelector = {
    var current = probabilitySelector
    for (kv <- compProductions)
      current = current.add(kv._2, kv._1)

    current
  }


  /**
   * Modifies a level.
   *
   * @param level Level to modify.
   * @return the level resulting from modification if one was performed, otherwise None.
   */
  def apply(level: L): Option[L] = {
    val matchingVertices = getMatch(level)
    val compProduction = productionProbabilitySelector.next(random)

    if (matchingVertices.isDefined) {
      Some(compProduction.apply(matchingVertices.get, level))
    } else {
      None
    }
  }
}

/**
 * Factory object for MultiProductions.
 */
object MultiProduction {

  /**
   * Constructs a new production, taking only the user-supplied classes.
   */
  def apply[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]]
          (pattern: Graph[PV, PE],
           matcher: GraphMatcher[R, C, PV, PE],
           compProductions: Vector[(ComponentProduction[L, R, C, PV], Int)],
           random: Random): MultiProduction[L, R, C, PV, PE] = new MultiProduction(pattern, compProductions, random, createIsomorphInspector(matcher), new MorphismFactory(), new ProbabilityCollection[ComponentProduction[L, R, C, PV]]())

  /**
   * Constructs a new multi production with default isomorphism and morphism factory, and the probability of each
   * ComponentProduction set to 1.0.
   *
   * @param compProductions A list of all ComponentProductions that this multi production may apply.
   * @param random Used to select ComponentProductions randomly.
   */
  def defaultProbability[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]]
          (pattern: Graph[PV, PE],
           matcher: GraphMatcher[R, C, PV, PE],
           random: Random,
           compProductions: ComponentProduction[L, R, C, PV]*): MultiProduction[L, R, C, PV, PE] = apply(pattern, matcher, compProductions.map(p => (p, 1)).toVector, random)

  /**
   * Constructs a new multi production by having the user specify all helper algorithms.
   *
   * @param pattern A graph whose vertices and edges matches the sought-after level topology to apply the production to.
   *                Edges will only be taken into account in the manner they connect vertices, not by checking
   *                edge-specific data. Must be a non-empty graph.
   * @param compProductions A list of all ComponentProductions that this multi production may apply, paired with
   *                        a value indicating the probability of that production to be selected. The probability
   *                        of an individual element is its value divided by the sum of all values.
   * @param random Used to select ComponentProductions randomly.
   * @param isomorphismMapper Examines every supplied levels graph for sub-structures that matches the pattern, and
   *                          returns one of them.
   * @param morphismFactory Factory responsible for building morphisms based on isomorphic mappings.
   * @param probabilitySelector Object responsible for randomly selecting which production to use.
   */
  def customSetup[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]]
            (pattern: Graph[PV, PE],
            compProductions: Vector[(ComponentProduction[L, R, C, PV], Int)],
            random: Random,
            isomorphismMapper: IsomorphicMapping[R, C, PV, PE],
            morphismFactory: MorphismFactory,
            probabilitySelector: RandomCollection[ComponentProduction[L, R, C, PV]]): MultiProduction[L, R, C, PV, PE] =
              new MultiProduction(pattern, compProductions, random, isomorphismMapper, morphismFactory, probabilitySelector)

  private def createIsomorphInspector[R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]]
  (m: GraphMatcher[R, C, PV, PE]): IsomorphicMapping[R, C, PV, PE] = {
    if (m.isEmpty) {
      new VF2IMatcher[R, C, PV, PE]()
    } else if (m.comparesVertices && !m.comparesEdges) {
      new VF2IMatcher[R, C, PV, PE](m.vertexComparator)
    } else if (!m.comparesVertices && m.comparesEdges) {
      new VF2IMatcher[R, C, PV, PE](m.edgeComparator)
    } else {
      new VF2IMatcher[R, C, PV, PE](m.vertexComparator, m.edgeComparator)
    }
  }
}
