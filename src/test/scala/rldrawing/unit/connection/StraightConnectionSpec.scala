package rldrawing.unit.connection

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}
import net.cyndeline.scalarlib.rldrawing.connection.StraightConnection
import testHelpers.SpecImports

class StraightConnectionSpec extends SpecImports {
  private val defaultConnection = new StraightConnection(999, 3, 999)

  describe("StraightConnection") {

    it ("should connect to equal neighbors") {

      Given("a rectangle R with neighbors having R's dimensions, positioned such that every neighbor has the same" +
            "x- or y coordinates as R")
      val r = Rectangle(Point(4, 5), Point(7, 8))
      val westNeighbor = Rectangle(Point(0, 5), Point(3, 8))
      val northNeighbor = Rectangle(Point(4, 9), Point(7, 12))
      val eastNeighbor = Rectangle(Point(8, 5), Point(11, 8))
      val southNeighbor = Rectangle(Point(4, 1), Point(7, 4))
      val allRooms = Vector(r, westNeighbor, northNeighbor, eastNeighbor, southNeighbor)

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), allRooms)

      Then("four connections should be added")
      connections should have size 4

      And("a connection between R and west should exist")
      connections should contain (Rectangle(Point(westNeighbor.stop.x, westNeighbor.start.y), Point(r.start.x, r.stop.y)))

      And("a connection between R and north should exist")
      connections should contain (Rectangle(Point(r.start.x, r.stop.y), Point(northNeighbor.stop.x, northNeighbor.start.y)))

      And("a connection between R and east should exist")
      connections should contain (Rectangle(Point(r.stop.x, r.start.y), Point(eastNeighbor.start.x, eastNeighbor.stop.y)))

      And("a connection between R and south should exist")
      connections should contain (Rectangle(Point(r.start.x, r.start.y), Point(southNeighbor.stop.x, southNeighbor.stop.y)))

    }

    it ("should connect to smaller neighbors") {

      Given("a rectangle R with a western side of length 5, and a neighbor whose eastern side is 3")
      val r = Rectangle(Point(4, 4), Point(7, 8))
      val n = Rectangle(Point(1, 5), Point(2, 7))

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, n))

      Then("a connection between R and N with size 3 should be created")
      connections should equal (Seq(Rectangle(Point(n.stop.x, n.start.y), Point(r.start.x, n.stop.y))))

    }

    it ("should connect to larger neighbors") {

      Given("a rectangle R with southern side having length 3, and a neighbor whose northern side is 5")
      val r = Rectangle(Point(4, 5), Point(6, 7))
      val n = Rectangle(Point(2, 2), Point(6, 4))

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, n))

      Then("a connection between R and N with size 3 should be created")
      connections should equal (Seq(Rectangle(Point(r.start.x, n.stop.y), Point(r.stop.x, r.start.y))))

    }

    it ("should connect to neighbors with negative coordinates") {

      Given("a rectangle R with a western neighbor with negative x coordinates and a southern neighbor negative y coordinates")
      val r = Rectangle(Point(4, 4), Point(6, 7))
      val westNeighbor = Rectangle(Point(-6, 4), Point(-3, 7))
      val southNeighbor = Rectangle(Point(4, -8), Point(6, -3))

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, westNeighbor, southNeighbor))

      Then("two connections should be added")
      connections should have size 2

      And("a connection between R and west should exist")
      connections should contain (Rectangle(Point(westNeighbor.stop.x, westNeighbor.start.y), Point(r.start.x, r.stop.y)))

      And("a connection between R and south should exist")
      connections should contain (Rectangle(Point(r.start.x, r.start.y), Point(southNeighbor.stop.x, southNeighbor.stop.y)))

    }

    it ("should connect a rectangle to neighbors when its coordinates are negative") {

      Given("a rectangle R with negative x coordinates connecting to a neighbor with positive coordinates")
      val r = Rectangle(Point(-8, 5), Point(-4, 7))
      val n = Rectangle(Point(2, 5), Point(4, 7)) // To the east

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, n))

      Then("a connection between R and N should be added")
      connections should contain (Rectangle(Point(r.stop.x, r.start.y), Point(n.start.x, n.stop.y)))

    }

    it ("should connect a rectangle to neighbors when both it and its coordinates are negative") {

      Given("two rectangles with negative y coordinates")
      val r = Rectangle(Point(4, -7), Point(6, -5))
      val n = Rectangle(Point(4, -3), Point(6, -1)) // North

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, n))

      Then("a connection between R and N should be added")
      connections should contain (Rectangle(Point(r.start.x, r.stop.y), Point(n.stop.x, n.start.y)))

    }

    it ("should not connect to neighbors covered by another neighbor") {

      Given("a rectangle R with an eastern neighbor that completely covers a rectangle even fruther east")
      val r = Rectangle(Point(1, 3), Point(3, 7))
      val eastNeighbor = Rectangle(Point(5, 3), Point(7, 8))
      val covered = Rectangle(Point(8, 2), Point(10, 5))

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, eastNeighbor, covered))

      Then("one connection should be added")
      connections should have size 1

      And("a connection between R and its closest eastern neighbor should be added")
      connections should contain (Rectangle(Point(r.stop.x, r.start.y), Point(eastNeighbor.start.x, r.stop.y)))

    }

    it ("should connect to a partially visible neighbor") {

      Given("a rectangle R with a fully visible eastern neighbor n, and another eastern neighbor partially covered by n")
      val r = Rectangle(Point(1, 3), Point(3, 7))
      val eastNeighbor = Rectangle(Point(5, 3), Point(7, 5))
      val partiallyCovered = Rectangle(Point(8, 4), Point(10, 7))

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, eastNeighbor, partiallyCovered))

      Then("two connections should be added")
      connections should have size 2

      And("a connection between R and its closest eastern neighbor should be added")
      connections should contain (Rectangle(Point(r.stop.x, r.start.y), Point(eastNeighbor.start.x, eastNeighbor.stop.y)))

      And("a connection between R and its covered eastern neighbor should be added")
      connections should contain (Rectangle(Point(3, 5), Point(partiallyCovered.start.x, partiallyCovered.stop.y)))

    }

    it ("should not connect to a partially visible neighbor where the visible segment is too small for a connection") {

      Given("a rectangle R with two fully visible eastern neighbors, with a gap of size 2 between them that makes a third neighbor visible")
      val r = Rectangle(Point(1, 1), Point(3, 7))
      val eastNeighbor1 = Rectangle(Point(4, 1), Point(6, 3))
      val eastNeighbor2 = Rectangle(Point(4, 4), Point(6, 7))
      val partiallyCovered = Rectangle(Point(7, 2), Point(9, 5))

      When("connecting r to its neighbors using a minimum connection size 3")
      assert(defaultConnection.minConnection >= 3)
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, eastNeighbor1, eastNeighbor2, partiallyCovered))

      Then("two connections should be added")
      connections should have size 2

      And("a connection between R and its two closest eastern neighbors should be added")
      connections should contain (Rectangle(Point(r.stop.x, r.start.y), Point(eastNeighbor1.start.x, eastNeighbor1.stop.y)))
      connections should contain (Rectangle(Point(r.stop.x, eastNeighbor2.start.y), Point(eastNeighbor2.start.x, eastNeighbor2.stop.y)))

    }

    it ("should connect multiple times to a neighbor with multiple visible segments") {

      Given("a rectangle R with a western neighbor N that is visible through three intervals created by two neighbors between R and N")
      val r = Rectangle(Point(7, 0), Point(9, 10))
      val n = Rectangle(Point(1, 0), Point(3, 10))
      val intermediateNeighbor1 = Rectangle(Point(4, 2), Point(6, 4))
      val intermediateNeighbor2 = Rectangle(Point(4, 6), Point(6, 8))

      When("connecting r to its neighbors using a minimum connection size 3")
      assert(defaultConnection.minConnection >= 3)
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, n, intermediateNeighbor1, intermediateNeighbor2))

      Then("five connections should be added")
      connections should have size 5

      And("a connection between R and its first intermediate neighbor should exist")
      connections should contain (Rectangle(Point(intermediateNeighbor1.stop.x, intermediateNeighbor1.start.y), Point(r.start.x, intermediateNeighbor1.stop.y)))

      And("a connection between R and its second intermediate neighbor should exist")
      connections should contain (Rectangle(Point(intermediateNeighbor2.stop.x, intermediateNeighbor2.start.y), Point(r.start.x, intermediateNeighbor2.stop.y)))

      And("three connections between R and N should exist")
      connections should contain (Rectangle(Point(n.stop.x, n.start.y), Point(r.start.x, intermediateNeighbor1.start.y)))
      connections should contain (Rectangle(Point(n.stop.x, intermediateNeighbor1.stop.y), Point(r.start.x, intermediateNeighbor2.start.y)))
      connections should contain (Rectangle(Point(n.stop.x, intermediateNeighbor2.stop.y), Point(r.start.x, r.stop.y)))

    }

    it ("should not create connections longer than the max connection size even if the connected sides allow it") {

      Given("a rectangle R facing a neighbor N with a side of length 11")
      val r = Rectangle(Point(7, 0), Point(9, 10))
      val n = Rectangle(Point(1, 0), Point(3, 10))

      When("connecting r to its neighbors using a maximum connection size 5")
      val maxConnection = new StraightConnection(999, 3, 5)
      val connections = maxConnection.addConnections(Vector(r), Vector(r, n))

      Then("one connection should be added")
      connections should have size 1

      And("that connection should be placed in the center of the two rectangles")
      connections should contain (Rectangle(Point(3, 3), Point(7, 7)))

    }

    it ("should take already-connected areas into consideration when computing cover") {

      Given("a rectangle R, with a neighbor N1 bridging it and another neighbor N2")
      val r = Rectangle(Point(1, 7), Point(3, 9))
      val n1 = Rectangle(Point(3, 7), Point(5, 9))
      val n2 = Rectangle(Point(5, 7), Point(7, 9))

      When("connecting r to its neighbors")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, n1, n2))

      Then("no connections should be possible")
      connections should be ('empty)

    }

    it ("should take visible but non-connected areas into consideration") {

      Given("a rectangle R and a neighbor N1 that blocks N2 from being connected, but cannot be connected itself")
      val r = Rectangle(Point(0, 0), Point(2, 3))
      val n1 = Rectangle(Point(3, 2), Point(5, 5))
      val n2 = Rectangle(Point(5, 1), Point(8, 3))

      When("connecting the neighbors of R")
      val connections = defaultConnection.addConnections(Vector(r), Vector(r, n1, n2))

      Then("no connections should be made")
      connections should be ('empty)

    }

  }

}
