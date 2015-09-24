package net.cyndeline.scalarlib.rldungeon.grammar.production

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rlgraph.subgraph.isomorphism.{IsomorphicMapping, NegativeCondition, ElementEquivalence}
import scala.util.Random
import net.cyndeline.scalarlib.rldungeon.grammar.util.{MorphismFactory, Morphism}
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import net.cyndeline.scalarlib.rlgraph.subgraph.isomorphism.jGraphT.SubGraphIsomorphismInspector
import scalax.collection.GraphPredef.OuterEdge
import net.cyndeline.scalarlib.util.{ProbabilityCollection, RandomCollection}
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Specifies a single left-hand side of a graph grammar, and multiple right-hand side ComponentProductions.
 * Allows random selection with probabilities between each ComponentProduction.
 *
 * @constructor Constructs a new multi-production.
 * @param pattern A graph whose vertices and edges matches the sought-after level topology to apply the production to.
 *                Edges will only be taken into account in the manner they connect vertices, not by checking
 *                edge-specific data. Must be a non-empty graph.
 * @param matcher Compares vertices in the pattern with vertices in the input levels graph to see if the graph contains
 *                a sub graph (a subsection of the level) whose topology matches the pattern.
 * @param negativeCondition Specifies properties that the supplied level is not allowed to have even though its
 *                          topology matched the pattern.
 * @param compProductions A list of all ComponentProductions that this multi production may apply, paired with
 *                        a value indicating the probability of that production to be selected. The probability
 *                        of an individual element is its value divided by the sum of all values.
 * @param random Used to select ComponentProductions randomly.
 * @param isomorphismMapper Examines every supplied levels graph for sub-structures that matches the pattern, and
 *                          returns one of them.
 * @param morphismFactory Factory responsible for building morphisms based on isomorphic mappings.
 * @param probabilitySelector Object responsible for randomly selecting which production to use.
 */
final class MultiProduction[L <: Level[L, R, C], R <: Room: TypeTag, C[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[R]]})#l : ({type l[M[_]] = ClassTag[M[R]]})#l] private
                                                             (pattern: Graph[R, C],
                                                              matcher: ElementEquivalence[R, C],
                                                              negativeCondition: Option[NegativeCondition[R, C]],
                                                              compProductions: Vector[(ComponentProduction[L, R, C], Double)],
                                                              random: Random,
                                                              isomorphismMapper: IsomorphicMapping,
                                                              morphismFactory: MorphismFactory,
                                                              probabilitySelector: RandomCollection[ComponentProduction[L, R, C]]
                                                              ) extends Production[L, R, C](pattern, matcher, negativeCondition, random, isomorphismMapper, morphismFactory) {

  val productionProbabilitySelector = probabilitySelector
  compProductions.foreach(kv => productionProbabilitySelector.add(kv._2, kv._1))

  /**
   * Constructs a new production, taking only the user-supplied classes and a negative condition.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           negativeCondition: NegativeCondition[R, C],
           compProductions: Vector[(ComponentProduction[L, R, C], Double)],
           random: Random) =
              this(pattern, matcher, Some(negativeCondition), compProductions, random, new SubGraphIsomorphismInspector(), new MorphismFactory(), new ProbabilityCollection[ComponentProduction[L, R, C]](random))

  /**
   * Constructs a new production, taking only the user-supplied classes and no negative condition.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           compProductions: Vector[(ComponentProduction[L, R, C], Double)],
           random: Random) =
              this(pattern, matcher, None, compProductions, random, new SubGraphIsomorphismInspector(), new MorphismFactory(), new ProbabilityCollection[ComponentProduction[L, R, C]](random))

  /**
   * Constructs a new multi production with default isomorphism and morphism factory, and the probability of each
   * ComponentProduction set to 1.0. Uses a negative condition.
   *
   * @param compProductions A list of all ComponentProductions that this multi production may apply.
   * @param random Used to select ComponentProductions randomly.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           negativeCondition: NegativeCondition[R, C],
           random: Random,
           compProductions: ComponentProduction[L, R, C]*) = this(pattern, matcher, negativeCondition, compProductions.map(p => (p, 1.0)).toVector, random)

  /**
   * Constructs a new multi production with default isomorphism and morphism factory, and the probability of each
   * ComponentProduction set to 1.0. Does not use a negative condition.
   *
   * @param compProductions A list of all ComponentProductions that this multi production may apply.
   * @param random Used to select ComponentProductions randomly.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           random: Random,
           compProductions: ComponentProduction[L, R, C]*) = this(pattern, matcher, compProductions.map(p => (p, 1.0)).toVector, random)

  /**
   * Constructs a new multi production by letting the user supply implementations of all underlying helper classes
   * and a negative condition.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           negativeCondition: NegativeCondition[R, C],
           compProductions: Vector[(ComponentProduction[L, R, C], Double)],
           random: Random,
           isomorphismMapper: IsomorphicMapping,
           morphismFactory: MorphismFactory,
           probabilitySelector: RandomCollection[ComponentProduction[L, R, C]]) = this(pattern, matcher, Some(negativeCondition), compProductions, random, isomorphismMapper, morphismFactory, probabilitySelector)

  /**
   * Constructs a new multi production by letting the user supply implementations of all underlying helper classes
   * and no negative condition.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           compProductions: Vector[(ComponentProduction[L, R, C], Double)],
           random: Random,
           isomorphismMapper: IsomorphicMapping,
           morphismFactory: MorphismFactory,
           probabilitySelector: RandomCollection[ComponentProduction[L, R, C]]) = this(pattern, matcher, None, compProductions, random, isomorphismMapper, morphismFactory, probabilitySelector)

  /**
   * Modifies a level.
   *
   * @param level Level to modify.
   * @return the level resulting from modification if one was performed, otherwise None.
   */
  def apply(level: L): Option[L] = {
    val matchingVertices: Option[Morphism[R]] = getMatch(level)
    val compProduction = productionProbabilitySelector.next

    if (matchingVertices.isDefined) {
      Some(compProduction.apply(matchingVertices.get, level))
    } else {
      None
    }
  }
}
