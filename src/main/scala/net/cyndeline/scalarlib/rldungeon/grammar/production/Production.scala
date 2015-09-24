package net.cyndeline.scalarlib.rldungeon.grammar.production

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rlgraph.subgraph.isomorphism.{IsomorphicMapping, NegativeCondition, ElementEquivalence}
import net.cyndeline.scalarlib.rldungeon.grammar.util.{MorphismFactory, Morphism}
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.util.Random
import scalax.collection.GraphPredef.OuterEdge
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Represents a modification performed on a level, done by first matching its graph against a pattern to see
 * if a matching topology exist, then by applying a component production to that sub-graph.
 *
 * @param pattern A graph whose vertices and edges matches the sought-after topology to apply the production to. Edges
 *                will only be taken into account in the manner they connect vertices, not by checking edge-specific
 *                data. Must be a non-empty graph.
 * @param matcher Compares vertices in the pattern with vertices in the input graph to see if the graph contains a
 *                sub graph whose topology matches the pattern.
 * @param negativeCondition Specifies properties that the supplied graph is not allowed to have even though its
 *                          topology matched the pattern.
 * @param random Used to select a random result if multiple topologies matching the pattern are found.
 * @param isomorphismMapper Examines every supplied graph for sub-structures that matches the pattern, and returns
 *                          one of them.
 * @param morphismFactory Factory responsible for building morphisms based on isomorphic mappings.
 * @tparam R Type of rooms used in level to apply production on.
 * @tparam C Type of corridors/connections used in level to apply production on.
 */
abstract class Production[L <: Level[L, R, C], R <: Room : TypeTag, C[X] <: UnDiEdge[X] : ({type l[M[_]] = TypeTag[M[R]]})#l : ({type l[M[_]] = ClassTag[M[R]]})#l]
 (pattern: Graph[R, C],
  matcher: ElementEquivalence[R, C],
  negativeCondition: Option[NegativeCondition[R, C]],
  random: Random,
  isomorphismMapper: IsomorphicMapping,
  morphismFactory: MorphismFactory) extends LevelProduction[L, R, C] {

  if (pattern.isEmpty) throw new IllegalArgumentException("The pattern supplied to a production cannot be empty.")

  /**
   * Constructs a production without a negative condition.
   */
  def this(pattern: Graph[R, C], matcher: ElementEquivalence[R, C], random: Random, isomorphismMapper: IsomorphicMapping, morphismFactory: MorphismFactory) =
    this(pattern, matcher, None, random, isomorphismMapper, morphismFactory)

  /**
   * Constructs a production with a negative condition.
   */
  def this(pattern: Graph[R, C], matcher: ElementEquivalence[R, C], negativeCondition: NegativeCondition[R, C], random: Random, isomorphismMapper: IsomorphicMapping, morphismFactory: MorphismFactory) =
    this(pattern, matcher, Some(negativeCondition), random, isomorphismMapper, morphismFactory)

  /**
   * Modifies a level.
   *
   * @param level Level to modify.
   * @return the level resulting from modification if one was performed, otherwise None.
   */
  def apply(level: L): Option[L]

  /**
   * Checks if a graph matches the production pattern, and returns a morphism.
   * @param level Level to match against pattern.
   * @return a morphism containing the pattern vertices, mapped against the vertices in the graph if a match was found.
   *         Otherwise None.
   */
  final def getMatch(level: L): Option[Morphism[R]] = {
    val isomorphismInspector: IsomorphicMapping = isomorphismMapper

    val mapping: Option[Map[R, R]] = isomorphismInspector.randomIsomorphicMapping[R, C](pattern, level.asGraph, matcher, random, negativeCondition)

    if (!mapping.isDefined)
      None
    else
      Some(morphismFactory.build(mapping.get))
  }

}
