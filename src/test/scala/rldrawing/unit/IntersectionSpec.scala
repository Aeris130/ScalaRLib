package rldrawing.unit

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldrawing.common.RectangleCoordinates
import net.cyndeline.scalarlib.rldrawing.util.{Intersection, Point}

@RunWith(classOf[JUnitRunner])
class IntersectionSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  describe("Intersection") {

    it ("should not find an intersection between two rectangles that doesn't share coordinates") {

      Given("two rectangles that doesn't intersect")
      val r1 = rectangle(3, 4, 7, 6)
      val r2 = rectangle(12, 13, 45, 57)

      When("computing the intersection")
      val intersection = Intersection(r1, r2)

      Then("no intersection should be found")
      assert(!intersection.intersects, "Intersection found between two non-overlapping rectangles.")

    }

    it ("should find an intersection between two rectangles sharing an edge along the y axis") {

      Given("two rectangles sharing the edge (5,5)->(5,9) along the y axis")
      val r1 = rectangle(3, 4, 5, 10)
      val r2 = rectangle(5, 5, 7, 9)

      When("computing the intersection")
      val intersection = Intersection(r1, r2)

      Then("the interval (5,5)->(5,9) should be found")
      intersection.start should be (Point(5, 5))
      intersection.stop should be (Point(5, 9))

    }

    it ("should find an intersection between two rectangles sharing an edge along the x axis") {

      Given("two rectangles sharing the edge (5,5)->(6,5) along the y axis")
      val r1 = rectangle(5, 2, 7, 5)
      val r2 = rectangle(3, 5, 6, 9)

      When("computing the intersection")
      val intersection = Intersection(r1, r2)

      Then("the interval (5,5)->(6,5) should be found")
      intersection.start should be (Point(5, 5))
      intersection.stop should be (Point(6, 5))

    }

    it ("should find an intersection between two rectangles sharing a single point") {

      Given("a rectangle (3,3)->(5,6) sharing the point (3,3) with the rectangle (0,0)->(3,3)")
      val r1 = rectangle(3, 3, 5, 6)
      val r2 = rectangle(0, 0, 3, 3)

      When("computing the intersection")
      val intersection = Intersection(r1, r2)

      Then("the interval (3,3) -> (3,3) should be found")
      intersection.start should be (Point(3, 3))
      intersection.stop should be (Point(3, 3))

    }

    it ("should compute intersections between two points") {

      Given("the 'rectangles' (3,3)->(3,3)")
      val r1 = rectangle(3, 3, 3, 3)
      val r2 = rectangle(3, 3, 3, 3)

      When("computing the intersection")
      val intersection = Intersection(r1, r2)

      Then("the interval (3,3) -> (3,3) should be found")
      intersection.start should be (Point(3, 3))
      intersection.stop should be (Point(3, 3))

    }

    it ("should throw an exception if start > stop for a rectangle") {

      Given("a rectangle with coordinates (3,3) and (2,2)")
      val validRectangle = rectangle(4, 4, 5, 5)

      When("computing the intersection")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        Intersection(rectangle(3, 3, 2, 2), validRectangle)
        Intersection(validRectangle, rectangle(3, 3, 2, 2))
      }

    }
  }

  private def rectangle(x1: Int, y1: Int, x2: Int, y2: Int) = new RectangleCoordinates {
    def start: Point = Point(x1, y1)
    def stop: Point = Point(x2, y2)
  }
}
