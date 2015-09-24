package rldrawing.unit.orthogonalGridCompaction

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.RectangularArea
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory.CorridorSegmentAreaFactory
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.util.Point

@RunWith(classOf[JUnitRunner])
class CorridorSegmentAreaFactorySpec extends FunSpec with GivenWhenThen with ShouldMatchers {
  private val factory = new CorridorSegmentAreaFactory()

  def smallEqualSizeAreasHorizontal = new {
    val area1 = RectangularArea(Point(1, 1), Point(3, 4))
    val area2 = RectangularArea(Point(5, 1), Point(7, 4))
  }

  def smallEqualSizeAreasVertical = new {
    val area1 = RectangularArea(Point(1, 1), Point(3, 4))
    val area2 = RectangularArea(Point(1, 7), Point(3, 10))
  }

  def largeEqualSizeAreasHorizontal = new {
    val area1 = RectangularArea(Point(1, 1), Point(6, 8))
    val area2 = RectangularArea(Point(7, 1), Point(12, 8))
  }

  describe("CorridorSegmentAreaFactory") {

    it ("should compute a segment between two areas of equal size [Horizontal]") {

      Given("two rectangular areas of size 3x4")
      val f = smallEqualSizeAreasHorizontal
      import f._

      When("connecting on the west/east sides with a corridor of width 4")
      val corridorSegment = factory.makeSegment(area1, East, area2, 4)

      Then("the segment should lie between the two areas at (3,1) and (5,4)")
      corridorSegment.start should be (Point(3, 1))
      corridorSegment.stop should be (Point(5, 4))

    }

    it ("should compute a segment between two areas of equal size [Vertical]") {

      Given("two rectangular areas of size 3x4")
      val f = smallEqualSizeAreasVertical
      import f._

      When("connecting on the north/south sides with a corridor of width 3")
      val corridorSegment = factory.makeSegment(area1, South, area2, 3)

      Then("the segment should lie between the two areas at (1,3) and (3,7)")
      corridorSegment.start should be (Point(1, 4))
      corridorSegment.stop should be (Point(3, 7))

    }

    it ("should position a segment in the middle of two same-size area boundaries [Evenly sized]") {

      Given("two areas of size 6x8 with start/stop at the same y coordinates")
      val f = largeEqualSizeAreasHorizontal
      import f._

      When("connecting on the west/east side with a corridor of width 4")
      val corridorSegment = factory.makeSegment(area1, East, area2, 4)

      Then("the corridor should lie in the middle on the y axis")
      corridorSegment.start should be (Point(6, 3))
      corridorSegment.stop should be (Point(7, 6))

    }

    it ("should position a segment in the middle of two same-size area boundaries [Odd sized]") {

      Given("two areas of size 6x8 with start/stop at the same y coordinates")
      val f = largeEqualSizeAreasHorizontal
      import f._

      When("connecting on the west/east side with a corridor of width 3 such that the coordinates can't be evenly split in half")
      val corridorSegment = factory.makeSegment(area1, East, area2, 3)

      Then("the corridor should either lie between 3 and 5 on the y axis, or between 4 and 6")
      assert((corridorSegment.start == Point(6, 3) && corridorSegment.stop == Point(7, 5))
        || (corridorSegment.start == Point(6, 4) && corridorSegment.stop == Point(7, 6)))

    }

    it ("should use the smallest boundary when computing segment position if the facing boundaries differ in size") {

      Given("an area with size 9 on the x axis")
      val area1 = RectangularArea(Point(1, 1), Point(9, 4))

      When("connecting a segment with width 3 on its southern side to an area with size 3 on the x axis")
      val area2 = RectangularArea(Point(2, 6), Point(4, 10))
      val corridorSegment = factory.makeSegment(area1, South, area2, 3)

      Then("the corridor should start and stop at the same x coordinates as the smaller area")
      corridorSegment.start should be (Point(2, 4))
      corridorSegment.stop should be (Point(4, 6))

    }

    it ("should connect two areas that intersect with a corridor of length 1") {

      Given("two areas that intersect on the x axis with size 5")
      val area2 = RectangularArea(Point(1, 1), Point(5, 6))
      val area1 = RectangularArea(Point(1, 6), Point(5, 10)) // below area 2

      When("connecting the areas with a segment of width 5")
      val corridorSegment = factory.makeSegment(area1, North, area2, 5)

      Then("the segment should have length 1 and width 5")
      corridorSegment.start should be (Point(1, 6))
      corridorSegment.stop should be (Point(5, 6))

    }

    /*
     * Exceptions
     */

    it ("should throw an exception if corridor width is specified as 0") {

      Given("two areas")
      val f = smallEqualSizeAreasHorizontal
      import f._

      When("attempting to connect them using a corridor of width 0")
      Then("an exception should be thrown")
      intercept[Error] {
        factory.makeSegment(area1, North, area2, 0)
      }

    }

  }
}
