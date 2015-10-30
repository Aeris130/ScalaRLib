package net.cyndeline.scalarlib.rldungeon.grammar.production

import net.cyndeline.rlgraph.subgraph.isomorphism.{IsomorphicMatch, ElementEquivalence, IsomorphicMapping}
import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.grammar.util.{GraphMatcher, Morphism, MorphismFactory}
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

/**
 * Represents a modification performed on a level, done by first matching its graph against a pattern to see
 * if a matching topology exist, then by applying a component production to that sub-graph.
 *
 * @param pattern A graph whose vertices and edges matches the sought-after topology to apply the production to. Edges
 *                will only be taken into account in the manner they connect vertices, not by checking edge-specific
 *                data. Must be a non-empty graph.
 * @param random Used to select a random result if multiple topologies matching the pattern are found.
 * @param isomorphismMapper Examines every supplied graph for sub-structures that matches the pattern, and returns
 *                          one of them.
 * @param morphismFactory Factory responsible for building morphisms based on isomorphic mappings.
 * @tparam R Type of rooms used in level to apply production on.
 * @tparam C Type of corridors/connections used in level to apply production on.
 * @tparam PV Vertex type used in pattern graphs.
 * @tparam PE Edge type used in pattern graphs.
 */
abstract class Production[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]]
 (pattern: Graph[PV, PE],
  random: Random,
  isomorphismMapper: IsomorphicMapping[R, C, PV, PE],
  morphismFactory: MorphismFactory) extends LevelProduction[L, R, C, PV] {

  if (pattern.isEmpty) throw new IllegalArgumentException("The pattern supplied to a production cannot be empty.")

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
  final def getMatch(level: L): Option[Morphism[R, PV]] = {
    val isomorphismInspector = isomorphismMapper

    val mapping: Option[IsomorphicMatch[R, PV]] = isomorphismInspector.randomIsomorphicMapping(level.asGraph, pattern, random)

    if (mapping.isEmpty)
      None
    else
      Some(morphismFactory.build(mapping.get.nodes))
  }

}
