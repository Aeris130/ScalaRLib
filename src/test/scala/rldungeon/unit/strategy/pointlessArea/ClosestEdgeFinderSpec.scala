package rldungeon.unit.strategy.pointlessArea

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.PointlessAreaData
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help.ClosestEdgeFinder

@RunWith(classOf[JUnitRunner])
class ClosestEdgeFinderSpec extends FunSpec with GivenWhenThen with ShouldMatchers {
  private val edgeFinder = new ClosestEdgeFinder()

  private def edges = new {
    val n1 = new CollapsedNode(1)
    val n2 = new CollapsedNode(2)
    val n3 = new CollapsedNode(3)
    val n4 = new CollapsedNode(4)
    val n5 = new CollapsedNode(5)
    val n6 = new CollapsedNode(6)
    val n7 = new CollapsedNode(7)
    val n8 = new CollapsedNode(8)

    // Valid edges that can hold responders, nodes doesn't matter as long as they're a unique combination
    val ve1 = CollapsedEdge(n1, n2)
    val ve2 = CollapsedEdge(n1, n3)
    val ve3 = CollapsedEdge(n1, n4)
    val ve4 = CollapsedEdge(n1, n5)
    val ve5 = CollapsedEdge(n1, n6)

    // Invalid edges that can't hold responders
    val ive1 = CollapsedEdge(n2, n3)
    val ive2 = CollapsedEdge(n2, n4)
    val ive3 = CollapsedEdge(n2, n5)
    val ive4 = CollapsedEdge(n2, n6)
    val ive5 = CollapsedEdge(n2, n7)

    // Dummy edges
    val de1 = CollapsedEdge.dummyEdge(n3, n4)
    val de2 = CollapsedEdge.dummyEdge(n3, n5)
    val de3 = CollapsedEdge.dummyEdge(n3, n6)
    val de4 = CollapsedEdge.dummyEdge(n3, n7)
    val de5 = CollapsedEdge.dummyEdge(n3, n8)

    val capacity = 1
    val areaData = new PointlessAreaData()
      .setResponderCapacity(1, 2, capacity)
      .setResponderCapacity(1, 3, capacity)
      .setResponderCapacity(1, 4, capacity)
      .setResponderCapacity(1, 5, capacity)
      .setResponderCapacity(1, 6, capacity)
  }

  describe("ClosestEdgeFinder") {

    it ("should return a single valid edge") {

      Given("a sequence with a single valid edge")
      val f = edges
      import f._
      val seq = Vector(Vector(ve1))

      When("finding a valid edge candidate")
      val edge = edgeFinder.findEdge(seq, areaData).get._1

      Then("the result should be the valid edge")
      edge should be (ve1)

    }

    it ("should return the edge closest to the beginning of the input sequence") {

      Given("a single sequence with two valid edges")
      val f = edges
      import f._
      val seq = Vector(Vector(ve1, ve2))

      When("finding a valid edge candidate")
      val edge = edgeFinder.findEdge(seq, areaData).get._1

      Then("the result should be the first valid edge")
      edge should be (ve1)

    }

    it ("should not return dummy edges") {

      Given("a sequence with a dummy edge followed by a regular edge")
      val f = edges
      import f._
      val seq = Vector(Vector(de1, ve1))

      When("finding a valid edge candidate")
      val edge = edgeFinder.findEdge(seq, areaData).get._1

      Then("the result should be the second edge")
      edge should be (ve1)

    }

    it ("should return None if no candidate is found") {

      Given("a single sequence with no valid edges")
      val f = edges
      import f._
      val seq = Vector(Vector(ive1, ive2))

      When("finding a valid edge candidate")
      val edge = edgeFinder.findEdge(seq, areaData)

      Then("the result should be None")
      edge should be (None)

    }

    it ("should return the edge closest to the beginning of a sequence if multiple sequences with valid edges exist") {

      Given("two sequences, were the first sequence has a valid edge at position 3, and the second has a valid edge at position 2")
      val f = edges
      import f._
      val seq = Vector(Vector(de1, de2, ve1), Vector(de3, ve2))

      When("finding a valid edge candidate")
      val edge = edgeFinder.findEdge(seq, areaData).get._1

      Then("the edge should be the second edge in the second sequence")
      edge should be (ve2)

    }

    it ("should update the data object if an edge is found") {

      Given("a sequence with a single valid edge")
      val f = edges
      import f._
      val seq = Vector(Vector(ve1))
      val from = ve1._1.singleRepresentedVertex
      val to = ve1._2.singleRepresentedVertex
      val initialResponderCapacity = areaData.getResponderCapacity(from, to)

      When("finding a valid edge candidate")
      val updatedData = edgeFinder.findEdge(seq, areaData).get._2

      Then("the responder capacity for the edge should be 1 less than before")
      updatedData.getResponderCapacity(from, to) should be (initialResponderCapacity - 1)

    }


  }
}
