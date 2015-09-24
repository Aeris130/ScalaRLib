package rldungeon.help

import net.cyndeline.scalarlib.rlgraph.subgraph.isomorphism.{NegativeCondition, ElementEquivalence, IsomorphicMapping}
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.util.Random

/**
 * Used instead of actual mocked objects since scalaMock doesn't play nice with context bound type tags for some reason.
 *
 * @param expected Arguments expected for the random mapping, in the expected order, along with the return value.
 */
class IsomorphicMappingMock[A : TypeTag, B[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[A]]})#l : ({type l[M[_]] = ClassTag[M[A]]})#l]
  (expected: Vector[((Graph[A, B], Graph[A, B], ElementEquivalence[A, B], Random, Option[NegativeCondition[A, B]]), Map[A, A])]) extends IsomorphicMapping {

  var currentExpectations = expected

  override def firstIsomorphicMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l : ({type l[M[_]] = ClassTag[M[VType]]})#l]
    (subGraph: Graph[VType, EType],
     graph: Graph[VType, EType],
     comparator: ElementEquivalence[VType, EType],
     negativeCondition: Option[NegativeCondition[VType, EType]])= ???  // not implemented

  override def randomIsomorphicMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l : ({type l[M[_]] = ClassTag[M[VType]]})#l]
    (subGraph: Graph[VType, EType],
     graph: Graph[VType, EType],
     comparator: ElementEquivalence[VType, EType],
     random: Random,
     negativeCondition: Option[NegativeCondition[VType, EType]]): Option[Map[VType, VType]] = {

    val expectedValues: (Graph[A, B], Graph[A, B], ElementEquivalence[A, B], Random, Option[NegativeCondition[A, B]]) = currentExpectations.head._1
    val returnCast = currentExpectations.head._2.asInstanceOf[Map[VType, VType]]
    val suppliedValues = (subGraph, graph, comparator, random, negativeCondition)

    if (suppliedValues == expectedValues) { // Comparing different generic types here
      currentExpectations = currentExpectations.drop(1)
      Option(returnCast)
    } else {
      throw new Error("The supplied values " + suppliedValues + " did not match " + expectedValues)
    }
  }

  def hasMetExpectations: Boolean = currentExpectations.isEmpty
}
