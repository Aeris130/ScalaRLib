package rldrawing.unit.orthogonalGridCompaction

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalamock.scalatest.MockFactory
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.RectangularArea
import scala.util.Random
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction.RandomTargetFinder
import net.cyndeline.scalarlib.rldrawing.util.Point

@RunWith(classOf[JUnitRunner])
class RandomTargetFinderSpec extends FunSpec with GivenWhenThen with ShouldMatchers with MockFactory {

  def random(x: Int, y: Int, area: RectangularArea): Random = {
    val rMock = mock[Random]
    (rMock.nextInt(_: Int)) expects(area.stop.x - area.start.x + 1) returns(x) once()
    (rMock.nextInt(_: Int)) expects(area.stop.y - area.start.y + 1) returns(y) once()
    rMock
  }
  describe("RandomTargetFinder") {

    it ("should return the lowest target if the random object returns 0") {

      Given("an area between (2,3) and (9,5) and a random object returning 0 on both axises")
      val area = new RectangularArea(Point(2, 3), Point(9, 5))
      val r = random(0, 0, area)

      When("finding a target")
      val finder = new RandomTargetFinder(r)
      val target = finder.findTarget(area)

      Then("the result should be (2, 3)")
      target should be (Point(2, 3))

    }

    it ("should return the highest target if the random object returns maximum x/y values - lower values") {

      Given("an area between (2,3) and (9,5) and a random object returning x = 9 - 2, y = 5 - 3")
      val area = new RectangularArea(Point(2, 3), Point(9, 5))
      val r = random(7, 2, area)

      When("finding a target")
      val finder = new RandomTargetFinder(r)
      val target = finder.findTarget(area)

      Then("the result should be (9, 5)")
      target should be (Point(9, 5))

    }

  }
}
