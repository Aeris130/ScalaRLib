package rldrawing.unit.orthogonalGridCompaction

import helperClasses.RandomMock
import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction.RandomTargetFinder
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.AdjustableRectangle
import testHelpers.SpecImports

import scala.util.Random

class RandomTargetFinderSpec extends SpecImports {

  def random(x: Int, y: Int, area: AdjustableRectangle): Random = {
    val rMock = RandomMock()
    rMock.expects(area.stop.x - area.start.x + 1) returns(x)
    rMock.expects(area.stop.y - area.start.y + 1) returns(y)
    rMock
  }
  describe("RandomTargetFinder") {

    it ("should return the lowest target if the random object returns 0") {

      Given("an area between (2,3) and (9,5) and a random object returning 0 on both axises")
      val area = new AdjustableRectangle(Point(2, 3), Point(9, 5))
      val r = random(0, 0, area)

      When("finding a target")
      val finder = new RandomTargetFinder(r)
      val target = finder.findTarget(area)

      Then("the result should be (2, 3)")
      target should be (Point(2, 3))

    }

    it ("should return the highest target if the random object returns maximum x/y values - lower values") {

      Given("an area between (2,3) and (9,5) and a random object returning x = 9 - 2, y = 5 - 3")
      val area = new AdjustableRectangle(Point(2, 3), Point(9, 5))
      val r = random(7, 2, area)

      When("finding a target")
      val finder = new RandomTargetFinder(r)
      val target = finder.findTarget(area)

      Then("the result should be (9, 5)")
      target should be (Point(9, 5))

    }

  }
}
