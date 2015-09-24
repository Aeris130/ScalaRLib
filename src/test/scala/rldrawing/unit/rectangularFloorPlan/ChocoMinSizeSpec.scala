package rldrawing.unit.rectangularFloorPlan

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge.UnDiEdge
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help.ChocoMinSize
import scalax.collection.GraphPredef._
import net.cyndeline.scalarlib.rldrawing.util.Intersection

@RunWith(classOf[JUnitRunner])
class ChocoMinSizeSpec extends FunSpec with GivenWhenThen with ShouldMatchers {
  private val miniSize = new ChocoMinSize()

  describe("ChocoMinSize") {

    it ("should increase a single rectangle") {

      Given("a layout with a single rectangle of size 3")
      val area = (1, (0, 0), (2, 2))
      val layout = new RectangularLayout(Vector(area), Graph[Int, UnDiEdge](1))

      When("increasing the layouts areas to a minimum of 5")
      val resized = miniSize.increaseToMinimum(layout, 5)

      Then("the rectangles area should be 5")
      val r = resized.rectangles(1)
      r.startX should be (0)
      r.startY should be (0)
      r.stopX should be (4)
      r.stopY should be (4)

    }

    it ("should increase three rectangles") {

      Given("a large rectangle with two small rectangles sharing its lower segment")
      val area1 = (1, (0, 0), (2, 2))
      val area2 = (2, (0, 2), (1, 3))
      val area3 = (3, (1, 2), (2, 3))
      val layout = new RectangularLayout(Vector(area1, area2, area3), Graph[Int, UnDiEdge](1~2, 2~3, 3~1))

      When("increasing the layouts areas to a minimum of 3")
      val resized = miniSize.increaseToMinimum(layout, 3)

      Then("area 1 should go from (0, 0) to (8, 4)")
      val r1 = resized.rectangles(1)
      r1.startX should be (0)
      r1.startY should be (0)
      r1.stopX should be (4)
      r1.stopY should be (2)

      And("area 2 should go from (0, 4) to (4, 8)")
      val r2 = resized.rectangles(2)
      r2.startX should be (0)
      r2.startY should be (2)
      r2.stopX should be (2)
      r2.stopY should be (4)

      And("area 3 should go from (4, 4) to (8, 8)")
      val r3 = resized.rectangles(3)
      r3.startX should be (2)
      r3.startY should be (2)
      r3.stopX should be (4)
      r3.stopY should be (4)

    }

    // Mostly to catch index out of bounds errors
    it ("should increase the size for layouts with more segments on one axis than the other") {

      Given("a layout with two vertical and three horizontal segments")
      val area1 = (1, (0, 0), (1, 1))
      val area2 = (2, (0, 1), (2, 2))
      val layout = new RectangularLayout(Vector(area1, area2), Graph[Int, UnDiEdge](1~2))

      When("increasing the layouts areas to a minimum of 3")
      val resized = miniSize.increaseToMinimum(layout, 3)

      Then("area 1 should go from (0, 0) to (2, 2)")
      val r1 = resized.rectangles(1)
      r1.startX should be (0)
      r1.startY should be (0)
      r1.stopX should be (2)
      r1.stopY should be (2)

      And("area 2 should go from (2, 0) to (4, 4)")
      val r2 = resized.rectangles(2)
      r2.startX should be (0)
      r2.startY should be (2)
      r2.stopX should be (2)
      r2.stopY should be (4)

    }

    it ("should increase the size of gates") {

      Given("a layout with two rectangles of size 2 with a gate of size 2 between them")
      val area1 = (1, (0, 0), (1, 1))
      val area2 = (2, (2, 0), (3, 1))
      val gate = ((1, 2), (1, 0), (2, 1))
      val layout = new RectangularLayout(Vector(area1, area2), Vector(gate), Graph[Int, UnDiEdge](1~2))

      When("increasing the layouts areas to a minimum of 3")
      val resized = miniSize.increaseToMinimum(layout, 3)

      Then("the gate should go from (1, 2) to (3, 3)")
      val g = resized.gates.head
      g.startX should be (2)
      g.startY should be (0)
      g.stopX should be (4)
      g.stopY should be (2)

    }

    it ("should not modify a rectangle that already fulfills the minimum size") {

      Given("a rectangle with sides 3x3")
      val area = (1, (0, 0), (2, 2))
      val layout = new RectangularLayout(Vector(area), Graph[Int, UnDiEdge](1))

      When("increasing the layouts areas to a minimum of 2")
      val resized = miniSize.increaseToMinimum(layout, 2)

      Then("the rectangle should retain its coordinates")
      val r1 = resized.rectangles(1)
      r1.startX should be (0)
      r1.startY should be (0)
      r1.stopX should be (2)
      r1.stopY should be (2)

    }

    it ("should keep a 1-coordinate margin between two segments affected by an adjacency constraint even if the minimum size has been reached") {

      Given("three adjacent areas that has reached the minimum size 2, but has segments that only lies 1 coordinate beside their adjacency constraints")
      val areaTopLeft = (1, (0, 0), (2, 1))
      val areaTopRight = (2, (2, 0), (4, 1))
      val middleBelow = (3, (1, 1), (3, 2))
      val layout = new RectangularLayout(Vector(areaTopLeft, areaTopRight, middleBelow), Graph[Int, UnDiEdge](3~1, 3~2))

      When("increasing the layouts areas to a size that have already reached")
      val resized = miniSize.increaseToMinimum(layout, 2)

      Then("the areas should be increased such that the intersections between both upper areas and the lower one have width >= 3")
      val topLeft = resized.rectangles(1)
      val topRight = resized.rectangles(2)
      val bottom = resized.rectangles(3)
      val leftIntersection = Intersection(topLeft.startX, topLeft.startY, topLeft.stopX, topLeft.stopY, bottom.startX, bottom.startY, bottom.stopX, bottom.stopY)
      val rightIntersection = Intersection(topRight.startX, topRight.startY, topRight.stopX, topRight.stopY, bottom.startX, bottom.startY, bottom.stopX, bottom.stopY)

      assert(leftIntersection.width >= 3, "The top left intersection had width " + leftIntersection.width + " rather than 3")
      assert(rightIntersection.width >= 3, "The top right intersection had width " + rightIntersection.width + " rather than 3")

    }

  }

}
