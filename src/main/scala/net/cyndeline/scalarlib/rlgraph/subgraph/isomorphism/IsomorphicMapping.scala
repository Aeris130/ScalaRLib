package net.cyndeline.scalarlib.rlgraph.subgraph.isomorphism

import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scala.util.Random
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

/**
 * Maps vertices in an isomorphic relation between two graphs.
 */
trait IsomorphicMapping {

  /**
   * Checks if a graph contains a subset of vertices and edges that matches those of a sub graph.
   *
   * @param subGraph Sub graph to check isomorphism for.
   * @param graph Graph to check if it contains the specified sub graph.
   * @param comparator Compares vertices for equivalence.
   * @param random Selects a random topology of the graph matching the sub graph, instead of
   *               the first one to be found.
   * @return a mapping between vertices in the sub graph and the graph it is a sub graph of, if such is the case.
   *         Otherwise None.
   */
  def randomIsomorphicMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l : ({type l[M[_]] = ClassTag[M[VType]]})#l]
                       (subGraph: Graph[VType, EType],
                        graph: Graph[VType, EType],
                        comparator: ElementEquivalence[VType, EType],
                        random: Random,
                        negativeCondition: Option[NegativeCondition[VType, EType]]): Option[Map[VType, VType]]

  /**
   * Checks if a graph contains a subset of vertices and edges that matches those of a sub graph, and returns
   * the first such topology that is found.
   *
   * @param subGraph Sub graph to check isomorphism for.
   * @param graph Graph to check if it contains the specified sub graph.
   * @param comparator Compares vertices for equivalence.
   * @return a mapping between vertices in the sub graph and the graph it is a sub graph of, if such is the case.
   *         Otherwise None.
   */
  def firstIsomorphicMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l : ({type l[M[_]] = ClassTag[M[VType]]})#l]
                       (subGraph: Graph[VType, EType],
                        graph: Graph[VType, EType],
                        comparator: ElementEquivalence[VType, EType],
                        negativeCondition: Option[NegativeCondition[VType, EType]]): Option[Map[VType, VType]]
}
