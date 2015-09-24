package rldrawing.unit.orthogonalGridCompaction

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.RectangularArea
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.util.Point

@RunWith(classOf[JUnitRunner])
class RectangularAreaSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  describe("RectangularArea") {

    it ("should throw an exception if start X is higher than stop X") {

      Given("a start coordinate with x value 1 and a stop coordinate with x value 0")
      val start = Point(1, 0)
      val stop = Point(0, 2)

      When("creating a rectangle")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        RectangularArea(start, stop)
      }

    }

    it ("should throw an exception if start Y is higher than stop Y") {

      Given("a start coordinate with y value 5 and a stop coordinate with y value 4")
      val start = Point(1, 5)
      val stop = Point(2, 4)

      When("creating a rectangle")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        RectangularArea(start, stop)
      }

    }

    it ("should compute the length of the north/south side") {

      Given("a rectangle with coordinates (1, 2) and (4, 6)")
      val rectangle = RectangularArea(Point(1, 2), Point(4, 6))

      When("computing the length of the north and south side")
      val northLength = rectangle.lengthOfSide(North)
      val southLength = rectangle.lengthOfSide(South)

      Then("the north and south length should be equal")
      northLength should equal (southLength)

      And("the sides should have length 4")
      northLength should be (4)

    }

    it ("should compute the length of the west/east side") {

      Given("a rectangle with coordinates (1, 2) and (4, 6)")
      val rectangle = RectangularArea(Point(1, 2), Point(4, 6))

      When("computing the length of the west and east side")
      val westLength = rectangle.lengthOfSide(West)
      val eastLength = rectangle.lengthOfSide(East)

      Then("the west and east length should be equal")
      westLength should equal (eastLength)

      And("the sides should have length 5")
      westLength should be (5)

    }

    it ("should compute start/stop coordinate values for the axis that modifies when traversing the north/south side") {

      Given("a rectangle with coordinates (1, 2) and (4, 6)")
      val rectangle = RectangularArea(Point(1, 2), Point(4, 6))

      When("computing coordinates for the north and south side")
      val northCoordinates = rectangle.coordinatesOnSide(North)
      val southCoordinates = rectangle.coordinatesOnSide(South)

      Then("the coordinates should be equal")
      northCoordinates should equal (southCoordinates)

      And("the coordinates should be 1 and 4")
      northCoordinates should be ((1, 4))

    }

    it ("should compute start/stop coordinate values for the axis that modifies when traversing the west/east side") {

      Given("a rectangle with coordinates (1, 2) and (4, 6)")
      val rectangle = RectangularArea(Point(1, 2), Point(4, 6))

      When("computing coordinates for the west and east side")
      val westCoordinates = rectangle.coordinatesOnSide(West)
      val eastCoordinates = rectangle.coordinatesOnSide(East)

      Then("the coordinates should be equal")
      westCoordinates should equal (eastCoordinates)

      And("the coordinates should be 2 and 6")
      westCoordinates should be ((2, 6))

    }

    it ("should adjust its coordinates north") {

      Given("a rectangle with coordinates (2, 3) and (4, 6)")
      val rectangle = RectangularArea(Point(2, 3), Point(4, 6))

      When("adjusting its coordinates north by 2")
      val newrectangle = rectangle.adjustCoordinates(North, 2)

      Then("the new rectangle should have its y-coordinates subtracted by 2")
      newrectangle.start should equal (Point(2, 1))
      newrectangle.stop should equal (Point(4, 4))

    }

    it ("should adjust its coordinates south") {

      Given("a rectangle with coordinates (2, 3) and (4, 6)")
      val rectangle = RectangularArea(Point(2, 3), Point(4, 6))

      When("adjusting its coordinates south by 2")
      val newrectangle = rectangle.adjustCoordinates(South, 2)

      Then("the new rectangle should have its y-coordinates added by 2")
      newrectangle.start should equal (Point(2, 5))
      newrectangle.stop should equal (Point(4, 8))

    }

    it ("should adjust its coordinates west") {

      Given("a rectangle with coordinates (2, 3) and (4, 6)")
      val rectangle = RectangularArea(Point(2, 3), Point(4, 6))

      When("adjusting its coordinates west by 2")
      val newrectangle = rectangle.adjustCoordinates(West, 2)

      Then("the new rectangle should have its x-coordinates subtracted by 2")
      newrectangle.start should equal (Point(0, 3))
      newrectangle.stop should equal (Point(2, 6))

    }

    it ("should adjust its coordinates east") {

      Given("a rectangle with coordinates (2, 3) and (4, 6)")
      val rectangle = RectangularArea(Point(2, 3), Point(4, 6))

      When("adjusting its coordinates east by 2")
      val newrectangle = rectangle.adjustCoordinates(East, 2)

      Then("the new rectangle should have its x-coordinates added by 2")
      newrectangle.start should equal (Point(4, 3))
      newrectangle.stop should equal (Point(6, 6))

    }

    it ("should check if a point is not inside the rectangle") {

      Given("a rectangle from (1,1) to (3,3)")
      val rectangle = RectangularArea(Point(1, 1), Point(3, 3))

      When("checking if the point (4,4) lies inside the rectangle")
      val isInside = rectangle.pointInside(Point(4, 4))

      Then("the result should be false")
      isInside should be (false)

    }

    it ("should check if a point on the edge of the rectangle is inside it") {

      Given("a rectangle from (1,1) to (3,3)")
      val rectangle = RectangularArea(Point(1, 1), Point(3, 3))

      When("checking if the point (1,2) lies inside the rectangle")
      val isInside = rectangle.pointInside(Point(1, 2))

      Then("the result should be true")
      isInside should be (true)

    }

    it ("should check if a point not touching the edge of the rectangle is inside it") {

      Given("a rectangle from (1,1) to (3,3)")
      val rectangle = RectangularArea(Point(1, 1), Point(3, 3))

      When("checking if the point (2,2) lies inside the rectangle")
      val isInside = rectangle.pointInside(Point(2, 2))

      Then("the result should be true")
      isInside should be (true)

    }

    it ("should compute start/stop coordinates for its northern side") {

      Given("a rectangle between (2, 3) and (8,9)")
      val rectangle = RectangularArea(Point(2, 3), Point(8,9))

      When("computing start/stop coordinates for its northern side")
      val startStop = rectangle.coordinatesForSide(North)

      Then("those coordinates should be (2,3) and (8,3)")
      startStop should be (Point(2, 3), Point(8, 3))

    }

    it ("should compute start/stop coordinates for its southern side") {

      Given("a rectangle between (2, 3) and (8,9)")
      val rectangle = RectangularArea(Point(2, 3), Point(8,9))

      When("computing start/stop coordinates for its southern side")
      val startStop = rectangle.coordinatesForSide(South)

      Then("those coordinates should be (2,9) and (8,9)")
      startStop should be (Point(2, 9), Point(8, 9))

    }

    it ("should compute start/stop coordinates for its western side") {

      Given("a rectangle between (2, 3) and (8,9)")
      val rectangle = RectangularArea(Point(2, 3), Point(8,9))

      When("computing start/stop coordinates for its western side")
      val startStop = rectangle.coordinatesForSide(West)

      Then("those coordinates should be (2,3) and (2,9)")
      startStop should be (Point(2, 3), Point(2, 9))

    }

    it ("should compute start/stop coordinates for its eastern side") {

      Given("a rectangle between (2, 3) and (8,9)")
      val rectangle = RectangularArea(Point(2, 3), Point(8,9))

      When("computing start/stop coordinates for its eastern side")
      val startStop = rectangle.coordinatesForSide(East)

      Then("those coordinates should be (8,3) and (8,9)")
      startStop should be (Point(8, 3), Point(8, 9))

    }

    it ("should reject another lower area as overlapping if they don't share coordinates") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(7, 8))

      When("checking if another rectangle with lower x and y values overlap it")
      val other = RectangularArea(Point(1, 2), Point(3, 4))
      val overlaps = rectangle.overlaps(other)

      Then("the result should be false")
      overlaps should be (false)

    }

    it ("should reject another higher area as overlapping if they don't share coordinates") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(7, 8))

      When("checking if another rectangle with higher x and y values overlap it")
      val other = RectangularArea(Point(9, 11), Point(12, 13))
      val overlaps = rectangle.overlaps(other)

      Then("the result should be false")
      overlaps should be (false)

    }

    it ("should accept another area as overlapping if it shares a single coordinate") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(7, 8))

      When("checking if another rectangle with stop x/y == the start x/y for the initial rectangle overlaps")
      val other = RectangularArea(Point(1, 1), Point(4, 5))
      val overlaps = rectangle.overlaps(other)

      Then("the result should be true")
      overlaps should be (true)

    }

    it ("should accept another area as overlapping if it covers the entire rectangle") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(7, 8))

      When("checking if another rectangle with lower start and higher stop overlaps")
      val other = RectangularArea(Point(1, 1), Point(9, 10))
      val overlaps = rectangle.overlaps(other)

      Then("the result should be true")
      overlaps should be (true)

    }

    it ("should accept another identical area as overlapping") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(7, 8))

      When("checking if an identical rectangle overlaps")
      val overlaps = rectangle.overlaps(rectangle)

      Then("the result should be true")
      overlaps should be (true)

    }

    it ("should accept another area as overlapping if it has some of its area inside the rectangle") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(7, 8))

      When("checking if another rectangle with some of its area overlapping overlaps")
      val other = RectangularArea(Point(5, 3), Point(8, 9))
      val overlaps = rectangle.overlaps(other)

      Then("the result should be true")
      overlaps should be (true)

    }

    it ("should reduce from its north side") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(8, 10))

      When("reducing from the north side by 3")
      val northReduced = rectangle.reduceFromSide(North, 3)

      Then("the resulting rectangle should have its starting y coordinate increased by 3")
      northReduced.start should be (Point(4, 8))

    }

    it ("should reduce from its south side") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(8, 10))

      When("reducing from the south side by 3")
      val southReduced = rectangle.reduceFromSide(South, 3)

      Then("the resulting rectangle should have its stop y coordinate reduced by 3")
      southReduced.stop should be (Point(8, 7))

    }

    it ("should reduce from its west side") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(8, 10))

      When("reducing from the west side by 3")
      val westReduced = rectangle.reduceFromSide(West, 3)

      Then("the resulting rectangle should have its starting x coordinate increased by 3")
      westReduced.start should be (Point(7, 5))

    }

    it ("should reduce from its east side") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(8, 10))

      When("reducing from the east side by 3")
      val eastReduced = rectangle.reduceFromSide(East, 3)

      Then("the resulting rectangle should have its stop x coordinate reduced by 3")
      eastReduced.stop should be (Point(5, 10))

    }

    it ("should compute intersections between areas that only intersect at the border") {

      Given("a rectangle A and a rectangle B that intersect on the north border of A")
      val A = RectangularArea(Point(1, 1), Point(5, 5))
      val B = RectangularArea(Point(0, 0), Point(4, 1))

      When("computing intersections")
      val intersection = A.intersection(B)

      Then("the intersection should be between point (1,1) and (1,4) on A")
      intersection should be ('defined)
      intersection.get should be (RectangularArea(Point(1, 1), Point(4, 1)))

    }

    it ("should compute intersections between areas that intersect inside their borders") {

      Given("a rectangle B that has half its area inside A")
      val A = RectangularArea(Point(1, 1), Point(8, 8))
      val B = RectangularArea(Point(2, 4), Point(4, 10))

      When("computing intersections")
      val intersection = A.intersection(B)

      Then("the intersection should be between point (2,4) and (4,8) on A")
      intersection should be ('defined)
      intersection.get should be (RectangularArea(Point(2, 4), Point(4, 8)))

    }

    it ("should reject intersections between areas that doesn't intersect") {

      Given("two areas that doesn't intersect")
      val A = RectangularArea(Point(4, 3), Point(6, 23))
      val B = RectangularArea(Point(333, 444), Point(555, 666))

      When("computing intersections")
      val intersection = A.intersection(B)

      Then("No intersection should be returned")
      intersection should be (None)

    }

    it ("should throw an exception when reducing by a negative amount") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(8, 10))

      When("reducing from the east side by -1")
      Then("an error should be thrown")
      intercept[Error] {
        rectangle.reduceFromSide(East, -1)
      }

    }

    it ("should throw an exception when reducing by 0") {

      Given("a rectangle")
      val rectangle = RectangularArea(Point(4, 5), Point(8, 10))

      When("reducing from the east side by 0")
      Then("an error should be thrown")
      intercept[Error] {
        rectangle.reduceFromSide(East, 0)
      }

    }

    it ("should not be a neighbor to areas further away than 1 coordinate") {

      Given("a rectangle A and another rectangle B that lies to the east of A and is further away than coordinate")
      val A = RectangularArea(Point(4, 5), Point(8, 10))
      val B = RectangularArea(Point(10, 5), Point(12, 10))

      When("Checking if A and B are neighbors")
      val isNeighbor = A.adjacentTo(B, East)

      Then("the result should be false")
      isNeighbor should be (false)

    }

    it ("should be neighbor to areas that lie 1 coordinate away from it in some direction and shares coordinates on the directions axis") {

      Given("a rectangle A and another rectangle B that lies 1 coordinate to the south of A and shares a coordinate on the x axis")
      val A = RectangularArea(Point(4, 5), Point(8, 10))
      val B = RectangularArea(Point(0, 11), Point(4, 13))

      When("Checking if A and B are neighbors")
      val isNeighbor = A.adjacentTo(B, South)

      Then("the result should be true")
      isNeighbor should be (true)

    }

    it ("should not be neighbor to areas that lie 1 coordinate away from it in some direction but doesn't share coordinates on the direction axis") {

      Given("a rectangle A and another rectangle B that lie 1 coordinate to the west of A but doesn't share any coordinates on the y axis")
      val A = RectangularArea(Point(4, 5), Point(8, 10))
      val B = RectangularArea(Point(0, 13), Point(3, 15))

      When("Checking if A and B are neighbors")
      val isNeighbor = A.adjacentTo(B, West)

      Then("the result should be false")
      isNeighbor should be (false)

    }

    it ("should not be neighbor to any area that lies south of it when the neighbors y coordinate is 0") {

      Given("a rectangle A")
      val A = RectangularArea(Point(4, 5), Point(8, 10))

      When("checking adjacency South of it to another rectangle on y-axis 0")
      val B = RectangularArea(Point(9, 0), Point(12, 15))
      val isNeighbor = A.adjacentTo(B, South)

      Then("the result should be false")
      isNeighbor should be (false)

    }

    it ("should not be neighbor to any area that lies east of it when the neighbors x coordinate is 0") {

      Given("a rectangle A")
      val A = RectangularArea(Point(4, 5), Point(8, 10))

      When("checking adjacency East of it to another rectangle on x-axis 0")
      val B = RectangularArea(Point(0, 1), Point(3, 3))
      val isNeighbor = A.adjacentTo(B, East)

      Then("the result should be false")
      isNeighbor should be (false)

    }


  }
}
