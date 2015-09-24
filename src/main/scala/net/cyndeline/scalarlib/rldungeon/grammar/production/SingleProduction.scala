package net.cyndeline.scalarlib.rldungeon.grammar.production

import net.cyndeline.rlgraph.subgraph.isomorphism.jGraphT.SubGraphIsomorphismInspector
import net.cyndeline.rlgraph.subgraph.isomorphism.{ElementEquivalence, IsomorphicMapping, NegativeCondition}
import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction
import net.cyndeline.scalarlib.rldungeon.grammar.util.{Morphism, MorphismFactory}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Specifies a single left-hand side in a graph grammar production, and a single right-hand side.
 *
 * @param pattern A graph whose vertices and edges matches the sought-after topology to apply the production to. Edges
 *                will only be taken into account in the manner they connect vertices, not by checking edge-specific
 *                data. Must be a non-empty graph.
 * @param matcher Compares vertices in the pattern with vertices in the input levels graph to see if the graph contains
 *                a sub graph (a subsection of the level) whose topology matches the pattern.
 * @param negativeCondition Specifies properties that the supplied level is not allowed to have even though its
 *                          topology matched the pattern.
 * @param compProduction Applies a modification to a level based on a morphism between the patterns vertices
 *                       and the levels rooms.
 * @param random Used to select ComponentProductions randomly.
 * @param isomorphismMapper Examines every supplied levels graph for sub-structures that matches the pattern, and
 *                          returns one of them.
 * @param morphismFactory Factory responsible for building morphisms based on isomorphic mappings.
 */
final class SingleProduction[L <: Level[L, R, C], R <: Room : TypeTag, C[X] <: UnDiEdge[X] : ({type l[M[_]] = TypeTag[M[R]]})#l : ({type l[M[_]] = ClassTag[M[R]]})#l] private
                                                              (pattern: Graph[R, C],
                                                               matcher: ElementEquivalence[R, C],
                                                               negativeCondition: Option[NegativeCondition[R, C]],
                                                               compProduction: ComponentProduction[L, R, C],
                                                               random: Random,
                                                               isomorphismMapper: IsomorphicMapping,
                                                               morphismFactory: MorphismFactory)
                                                               extends Production[L, R, C](pattern, matcher, negativeCondition, random, isomorphismMapper, morphismFactory) {

  /**
   * Constructs a new production using default pattern matching objects.
   *
   * @param pattern A graph whose vertices and edges matches the sought-after topology to apply the production to. Edges
   *                will only be taken into account in the manner they connect vertices, not by checking edge-specific
   *                data. Must be a non-empty graph.
   * @param matcher Compares vertices in the pattern with vertices in the input graph to see if the graph contains a
   *                sub graph whose topology matches the pattern.
   * @param negativeCondition Specifies properties that the supplied graph is not allowed to have even though its
   *                          topology matched the pattern.
   * @param compProduction Applies a modification to a graph based on a morphism between the patterns vertices
   *                       and the graph.
   * @param random Used to select ComponentProductions randomly.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           negativeCondition: Option[NegativeCondition[R, C]],
           compProduction: ComponentProduction[L, R, C],
           random: Random) = this(pattern, matcher, negativeCondition, compProduction, random, new SubGraphIsomorphismInspector(), new MorphismFactory())

  /**
   * Constructs a new production, taking only the user-supplied classes without a negative condition.
   *
   * @param pattern A graph whose vertices and edges matches the sought-after topology to apply the production to. Edges
   *                will only be taken into account in the manner they connect vertices, not by checking edge-specific
   *                data. Must be a non-empty graph.
   * @param matcher Compares vertices in the pattern with vertices in the input graph to see if the graph contains a
   *                sub graph whose topology matches the pattern.
   * @param compProduction Applies a modification to a graph based on a morphism between the patterns vertices
   *                       and the graph.
   * @param random Used to select ComponentProductions randomly.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           compProduction: ComponentProduction[L, R, C],
           random: Random) = this(pattern, matcher, None, compProduction, random, new SubGraphIsomorphismInspector(), new MorphismFactory())

  /**
   * Constructs a new single production by letting the user supply implementations of all underlying helper classes
   * and a negative condition.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           negativeCondition: NegativeCondition[R, C],
           compProduction: ComponentProduction[L, R, C],
           random: Random,
           isomorphismMapper: IsomorphicMapping,
           morphismFactory: MorphismFactory) = this(pattern, matcher, Some(negativeCondition), compProduction, random, isomorphismMapper, morphismFactory)

  /**
   * Constructs a new single production by letting the user supply implementations of all underlying helper classes
   * and no negative condition.
   */
  def this(pattern: Graph[R, C],
           matcher: ElementEquivalence[R, C],
           compProduction: ComponentProduction[L, R, C],
           random: Random,
           isomorphismMapper: IsomorphicMapping,
           morphismFactory: MorphismFactory) = this(pattern, matcher, None, compProduction, random, isomorphismMapper, morphismFactory)

  /**
   * Modifies a level.
   *
   * @param level Level to modify.
   * @return the level resulting from modification if one was performed, otherwise None.
   */
  def apply(level: L): Option[L] = {
    val matchingVertices: Option[Morphism[R]] = getMatch(level)

    if (matchingVertices.isDefined) {
      Some(compProduction.apply(matchingVertices.get, level))
    } else {
      None
    }
  }
}
