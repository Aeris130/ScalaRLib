package rldrawing.unit.orthogonalGridCompaction

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalamock.scalatest.MockFactory
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{RoomCorridorConnection, RectangularArea, Area, MutableArea}
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.util.{Point, Direction}

@RunWith(classOf[JUnitRunner])
class AreaSpec extends FunSpec with GivenWhenThen with ShouldMatchers with MockFactory {

  class AreaClass(area: RectangularArea, intersect: Boolean = true) extends Area(area, intersect) {
    override def isRoom: Boolean = ???
    override def isCorridor: Boolean = ???
    override def isBend: Boolean = ???
    override def canMove(direction: Direction.Direction): Set[MutableArea] = ???
    override def move(): Unit = ???
  }

  describe("Area") {

    it ("should not return adjacent areas found in connections") {

      Given("an area A with a connection to another area B, and a grid that returns B as adjacent to A")
      val grid = mock[PartitionedArea[MutableArea]]
      val rectA = RectangularArea(Point(0, 0), Point(3, 3))
      val rectB = RectangularArea(Point(3, 0), Point(5, 3))
      val A = new AreaClass(rectA)
      val B = new AreaClass(rectB)
      val AtoBConnection = mock[RoomCorridorConnection]
      A.connect(East, AtoBConnection)

      (AtoBConnection.corridor _) expects() returns(B) anyNumberOfTimes()
      (AtoBConnection.room _) expects() returns(A) anyNumberOfTimes()
      (grid.elementsIn _) expects(*, *) returns(Set(B)) anyNumberOfTimes()

      When("looking for adjacent non-connected areas to the right of the area A")
      val adjacent = A.adjacentNonConnectedAreas(East, grid)

      Then("The corridor B should not be returned")
      adjacent should be ('empty)

    }
  }
}
