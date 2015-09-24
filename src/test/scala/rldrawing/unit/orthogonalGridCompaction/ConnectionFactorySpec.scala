package rldrawing.unit.orthogonalGridCompaction

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.Point
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory.ConnectionFactory
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{Connection, CorridorArea, RectangularArea, RoomArea}
import testHelpers.SpecImports

class ConnectionFactorySpec extends SpecImports {
  private val factory = new ConnectionFactory()

  def twoAreasAndACorridor = new {
    val area1 = new RoomArea(RectangularArea(Point(0, 0), Point(4, 4)), null, true)
    val area2 = new RoomArea(RectangularArea(Point(5, 0), Point(9, 4)), null, true)
    val corridor = new CorridorArea(RectangularArea(Point(4, 0), Point(5, 4)), null, West, 1, true)
  }

  describe("ConnectionFactory") {

    it ("should connect the initial area to the corridor") {

      Given("two areas and a corridor going west from the initial area")
      val f = twoAreasAndACorridor
      import f._

      When("connecting the three areas")
      factory.connect(area1, corridor, area2, West, 0)

      Then("area 1 should have a western connection to the corridor")
      val connection = area1.connection(West)
      connection should be ('defined)
      connection.get.corridor should be (corridor)
      connection.get.room should be (area1)

      And("the corridor should contain the same connection object as area 1")
      corridor.connection(East) should be ('defined)
      corridor.connection(East).get should equal (connection.get)

      And("the direction inside the connection should point to the western side of area 1")
      connection.get.asInstanceOf[Connection].roomConnectDirection should be (West)

    }

    it ("should connect the second area to the corridor") {

      Given("two areas and a corridor going west from the initial area")
      val f = twoAreasAndACorridor
      import f._

      When("connecting the three areas")
      factory.connect(area1, corridor, area2, West, 0)

      Then("area 2 should have an eastern connection to the corridor")
      val connection = area2.connection(East)
      connection should be ('defined)
      connection.get.corridor should be (corridor)
      connection.get.room should be (area2)

      And("the corridor should contain the same connection object as area 1")
      corridor.connection(West) should be ('defined)
      corridor.connection(West).get should equal (connection.get)

      And("the direction inside the connection should point to the eastern side of area 2")
      connection.get.asInstanceOf[Connection].roomConnectDirection should be (East)

    }

    it ("should add deviation to connections") {

      Given("two areas and a corridor going west from the initial area")
      val f = twoAreasAndACorridor
      import f._

      When("connecting the three areas using a deviation of 4")
      factory.connect(area1, corridor, area2, West, 4)

      Then("both connection objects should have a deviation of 4")
      val connectionToArea1 = area1.connection(West).get.asInstanceOf[Connection]
      val connectionToArea2 = area2.connection(East).get.asInstanceOf[Connection]
      connectionToArea1.deviation should be (4)
      connectionToArea2.deviation should be (4)

    }
  }
}
