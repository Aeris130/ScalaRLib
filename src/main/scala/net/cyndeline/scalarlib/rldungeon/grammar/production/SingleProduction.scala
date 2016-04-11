package net.cyndeline.scalarlib.rldungeon.grammar.production

import net.cyndeline.rlgraph.subgraph.isomorphism.proofProcess.VF2IMatcher
import net.cyndeline.rlgraph.subgraph.isomorphism.IsomorphicMapping
import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction
import net.cyndeline.scalarlib.rldungeon.grammar.util.{GraphMatcher, Morphism, MorphismFactory}
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

/**
 * Specifies a single left-hand side in a graph grammar production, and a single right-hand side.
 *
 * @param pattern A graph whose vertices and edges matches the sought-after topology to apply the production to. Edges
 *                will only be taken into account in the manner they connect vertices, not by checking edge-specific
 *                data. Must be a non-empty graph.
 * @param compProduction Applies a modification to a level based on a morphism between the patterns vertices
 *                       and the levels rooms.
 * @param random Used to select ComponentProductions randomly.
 * @param isomorphismMapper Examines every supplied levels graph for sub-structures that matches the pattern, and
 *                          returns one of them.
 * @param morphismFactory Factory responsible for building morphisms based on isomorphic mappings.
 */
final class SingleProduction[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]] private
                                                              (pattern: Graph[PV, PE],
                                                               compProduction: ComponentProduction[L, R, C, PV],
                                                               random: Random,
                                                               isomorphismMapper: IsomorphicMapping[R, C, PV, PE],
                                                               morphismFactory: MorphismFactory)
                                                               extends Production[L, R, C, PV, PE](pattern, random, isomorphismMapper, morphismFactory) {

  /**
   * Modifies a level.
   *
   * @param level Level to modify.
   * @return the level resulting from modification if one was performed, otherwise None.
   */
  def apply(level: L): Option[L] = {
    val matchingVertices: Option[Morphism[R, PV]] = getMatch(level)

    if (matchingVertices.isDefined) {
      Some(compProduction.apply(matchingVertices.get, level))
    } else {
      None
    }
  }
}

/**
 * Factory object for SingleProductions.
 */
object SingleProduction {

  /**
   * Constructs a new production using default pattern matching objects.
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
  def apply[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]]
          (pattern: Graph[PV, PE],
           matcher: GraphMatcher[R, C, PV, PE],
           compProduction: ComponentProduction[L, R, C, PV],
           random: Random): SingleProduction[L, R, C, PV, PE] = new SingleProduction(pattern, compProduction, random, createIsomorphInspector(matcher), new MorphismFactory())

  /**
   * Constructs a new production using only user-specified helper objects.
   *
   * @param pattern A graph whose vertices and edges matches the sought-after topology to apply the production to. Edges
   *                will only be taken into account in the manner they connect vertices, not by checking edge-specific
   *                data. Must be a non-empty graph.
   * @param compProduction Applies a modification to a level based on a morphism between the patterns vertices
   *                       and the levels rooms.
   * @param random Used to select ComponentProductions randomly.
   * @param isomorphismMapper Examines every supplied levels graph for sub-structures that matches the pattern, and
   *                          returns one of them.
   * @param morphismFactory Factory responsible for building morphisms based on isomorphic mappings.
   */
  def apply[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV, PE[X] <: EdgeLikeIn[X]]
           (pattern: Graph[PV, PE],
            compProduction: ComponentProduction[L, R, C, PV],
            random: Random,
            isomorphismMapper: IsomorphicMapping[R, C, PV, PE],
            morphismFactory: MorphismFactory): SingleProduction[L, R, C, PV, PE] = new SingleProduction(pattern, compProduction, random, isomorphismMapper, morphismFactory)

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
