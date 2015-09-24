package rldungeon.unit.strategy

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import rldungeon.help.RoomVertex
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.algorithms.CollapsedGraphEdgeTrimmer
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.PointlessAreaData

@RunWith(classOf[JUnitRunner])
class CollapsedGraphEdgeTrimmerSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  def fixture = new {
    val trimmer = new CollapsedGraphEdgeTrimmer()

    // Non-pointless rooms
    val room1 = 1
    val room2 = 2
    val room3 = 3
    val room4 = 4
    val room5 = 5
    val room6 = 6
    val room7 = 7
    val room8 = 8
    val room9 = 9

    val n1 = new CollapsedNode(Set(room1))
    val n2 = new CollapsedNode(Set(room2))
    val n3 = new CollapsedNode(Set(room3))
    val n4 = new CollapsedNode(Set(room4))
    val n5 = new CollapsedNode(Set(room5))
    val n6 = new CollapsedNode(Set(room6))
    val n7 = new CollapsedNode(Set(room7))
    val n8 = new CollapsedNode(Set(room8))
    val n9 = new CollapsedNode(Set(room9))


    val e1 = CollapsedEdge(n1, n2)
    val e2 = CollapsedEdge(n2, n3)
    val e3 = CollapsedEdge(n3, n4)

    val mainPath = Graph(e1, e2, e3)
    val mainPathSet = Set(n1, n2, n3, n4)

    def evaluator(removable: Set[CollapsedNode])(n: CollapsedNode): Boolean = {
      !removable.contains(n)
    }

  }

  describe("CollapsedGraphEdgeTrimmer") {

    it ("should remove all nodes if the evaluator rejects them") {

      Given("a graph consisting nodes 1, 2, 3, 4")
      val f = fixture
      import f._
      val e1 = CollapsedEdge(n1, n2)
      val e2 = CollapsedEdge(n2, n3)
      val e3 = CollapsedEdge(n2, n4)
      val graph = Graph(e1, e2, e3)

      When("removing nodes using an evaluator that yield false for every noe")
      val trimmedGraph = trimmer.removeVertices(graph, Set(), evaluator(Set(n1, n2, n3, n4)))

      Then("the resulting graph should be empty")
      trimmedGraph should equal (Graph[CollapsedNode, CollapsedEdge]())

    }

    it ("should do nothing with a graph where all outermost nodes are accepted by the evaluator") {

      Given("a graph with a path 1-5-6 and 3-7-8")
      val f = fixture
      import f._
      val e1 = CollapsedEdge(n1, n5)
      val e2 = CollapsedEdge(n5, n6)
      val e3 = CollapsedEdge(n3, n7)
      val e4 = CollapsedEdge(n7, n8)
      val graph = mainPath + e1 + e2 + e3 + e4

      When("removing nodes using an evaluator that fails nodes 5 and 7")
      val trimmedGraph = trimmer.removeVertices(graph, mainPathSet, evaluator(Set(n5, n7)))

      Then("the graph should remain unchanged")
      trimmedGraph should equal (graph)

    }

    it ("should remove a single outermost node that is rejected by the evaluator") {

      Given("a main path connected to a single node")
      val f = fixture
      import f._
      val e1 = CollapsedEdge(n2, n5)
      val graph = mainPath + e1

      When("removing nodes using an evaluator that fails nodes 5")
      val trimmedGraph = trimmer.removeVertices(graph, mainPathSet, evaluator(Set(n5)))

      Then("the resulting graph should only contain the main path")
      trimmedGraph should equal (mainPath)

    }

    it ("should remove a single outermost node from a branch with more than one node") {

      Given("a main path connected to a path with two nodes")
      val f = fixture
      import f._
      val e1 = CollapsedEdge(n2, n5)
      val e2 = CollapsedEdge(n5, n6)
      val graph = mainPath + e1 + e2

      When("removing nodes using an evaluator that fails nodes 6")
      val trimmedGraph = trimmer.removeVertices(graph, mainPathSet, evaluator(Set(n6)))

      Then("the resulting graph should not contain node 6")
      trimmedGraph should equal (mainPath + e1)

    }

    it ("should remove multiple nodes that cannot hold rewards that are linked to each other and an outermost node") {

      Given("a main path with connected to a path with 3 nodes, 2-5-6-7")
      val f = fixture
      import f._
      val e1 = CollapsedEdge(n2, n5)
      val e2 = CollapsedEdge(n5, n6)
      val e3 = CollapsedEdge(n6, n7)

      val graph = mainPath + e1 + e2 + e3

      When("removing nodes using an evaluator that fails nodes 6 and 7")
      val trimmedGraph = trimmer.removeVertices(graph, mainPathSet, evaluator(Set(n6, n7)))

      Then("the resulting graph should have both nodes removed")
      trimmedGraph should equal (mainPath + e1)

    }

    it ("should only remove nodes until an intersection with degree > 2 is traversed") {

      Given("a main path connected to a path that branches in two (5 -> 6, 7)")
      val f = fixture
      import f._
      val e1 = CollapsedEdge(n2, n5)
      val e2 = CollapsedEdge(n5, n6)
      val e3 = CollapsedEdge(n5, n7)

      val graph = mainPath + e1 + e2 + e3

      When("removing nodes using an evaluator that fails nodes 6")
      val trimmedGraph = trimmer.removeVertices(graph, mainPathSet, evaluator(Set(n6)))

      Then("node 7 should not be removed since it connects to another node that cannot be removed")
      trimmedGraph should equal (mainPath + e1 + e3)

    }
  }
}
