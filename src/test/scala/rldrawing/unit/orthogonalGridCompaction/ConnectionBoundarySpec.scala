package rldrawing.unit.orthogonalGridCompaction

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.Point
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.help.ConnectionBoundary
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.RectangularArea
import testHelpers.SpecImports

class ConnectionBoundarySpec extends SpecImports {

  describe("ConnectionBoundary") {

    it ("should throw an exception if the connecting area already breaks the boundary start coordinate") {

      Given("a connecting area with start x coordinate 1 and a boundary with start x coordinate 2")
      val boundary = new RectangularArea(Point(2, 2), Point(4, 4))
      val connectingArea = new RectangularArea(Point(1, 1), Point(2, 4))

      When("computing connection boundaries")
      val connectionBoundary = new ConnectionBoundary(boundary)

      Then("an exception should be thrown")
      intercept[Error] {
        connectionBoundary.computeConnectionBoundary(connectingArea, North)
      }

    }

    it ("should throw an exception if the connecting area already breaks the boundary stop coordinate") {

      Given("a connecting area with stop y coordinate 5 and a boundary with stop y coordinate 4")
      val boundary = new RectangularArea(Point(2, 2), Point(4, 4))
      val connectingArea = new RectangularArea(Point(1, 2), Point(2, 5))

      When("computing connection boundaries")
      val connectionBoundary = new ConnectionBoundary(boundary)

      Then("an exception should be thrown")
      intercept[Error] {
        connectionBoundary.computeConnectionBoundary(connectingArea, West)
      }

    }

    /*
     *
     *  Tests with both areas having odd size
     *
     */

    it ("should compute the boundaries for two areas with identical odd sides to be the coordinates of the boundary") {

      Given("a boundary with a north side going from x = 2 to x = 4 (size 3) and a connecting area with the same x coordinates")
      val connectingArea = new RectangularArea(Point(2, 0), Point(4, 3))
      val boundary = new RectangularArea(Point(2, 3), Point(4, 6))

      When("computing connection boundaries")
      val connectionBoundary = new ConnectionBoundary(boundary)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, North)

      Then("the boundary coordinates should be 2 and 4")
      coordinates should be (2, 4)

    }

    it ("should compute the boundaries for an area larger than the connecting area (both having odd size) to be the size of the connecting area + the max deviation") {

      Given("a boundary with its west side having size 11 (0 -> 10) and a connecting area with east size 3")
      val connectingArea = new RectangularArea(Point(0, 0), Point(1, 2))
      val boundary = new RectangularArea(Point(1, 0), Point(2, 10))

      When("computing connection boundaries with a deviation of 2")
      val deviation = 2
      val connectionBoundary = new ConnectionBoundary(boundary, deviation)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be based on the middle coordinate in the boundary (5) +- half the connecting area size (1) and the deviation (2)")
      val start = 5 - 1 - deviation
      val stop = 5 + 1 + deviation
      coordinates should be ((start, stop))

    }

    it ("should use the boundary coordinates if the connecting areas size + 2*deviation is equal to the boundary area size and both sides have odd length") {

      Given("a boundary area with its west side having size 5 (1 -> 5) and a connecting area with size 3 (1 -> 3)")
      val connectStart = 1
      val connectStop = 3
      val connectingArea = new RectangularArea(Point(0, connectStart), Point(1, connectStop))

      val boundaryStart = 1
      val boundaryStop = 5
      val boundary = new RectangularArea(Point(1, boundaryStart), Point(2, boundaryStop))

      When("computing connection boundaries with a deviation of 1")
      val deviation = 1
      val connectionBoundary = new ConnectionBoundary(boundary, deviation)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be 1 and 5")
      coordinates should be ((boundaryStart, boundaryStop))

    }

    it ("should use the boundary coordinates if the connecting area size + deviation exceeds the boundary when both areas have odd size") {

      Given("a boundary with its west side having size 11 (0 -> 10) and a connecting area with east size 3")
      val connectingArea = new RectangularArea(Point(0, 0), Point(1, 2))
      val boundary = new RectangularArea(Point(1, 0), Point(2, 10))

      When("computing connection boundaries with a deviation of 99")
      val deviation = 99
      val connectionBoundary = new ConnectionBoundary(boundary, deviation)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be 0 and 10")
      coordinates should be ((0, 10))

    }

    /*
     *
     *  Tests with both areas having even size
     *
     */

    it ("should compute the boundaries for two areas with identical even sides to be the coordinates of the boundary") {

      Given("a boundary with a north side going from x = 1 to x = 4 (size 4) and a connecting area with the same x coordinates")
      val connectingArea = new RectangularArea(Point(1, 0), Point(4, 3))
      val boundary = new RectangularArea(Point(1, 3), Point(4, 6))

      When("computing connection boundaries")
      val connectionBoundary = new ConnectionBoundary(boundary)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, North)

      Then("the boundary coordinates should be 1 and 4")
      coordinates should be (1, 4)

    }

    it ("should compute the boundaries for an area larger than the connecting area (both having even size) to be the size of the connecting area + the max deviation") {

      Given("a boundary with its west side having size 10 (1 -> 10) and a connecting area with east size 4 (1 -> 4)")
      val connectingArea = new RectangularArea(Point(0, 1), Point(1, 4))
      val boundary = new RectangularArea(Point(1, 1), Point(2, 10))

      When("computing connection boundaries with a deviation of 1")
      val deviation = 1
      val connectionBoundary = new ConnectionBoundary(boundary, deviation)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be the half the boundary size +- half the connecting areas size +- the deviation")

      // Start coordinate + half the boundary size, - half the connecting size - deviation
      val start = 3

      // Stop coordinate - half the boundary size, + half the connecting size + deviation
      val stop = 8

      coordinates should be ((start, stop))

    }

    it ("should use the boundary coordinates if the connecting areas size + 2*deviation is equal to the boundary area size and both sides have even length") {

      Given("a boundary area with its west side having size 6 (1 -> 6) and a connecting area with size 4 (1 -> 4)")
      val connectStart = 1
      val connectStop = 4
      val connectingArea = new RectangularArea(Point(0, connectStart), Point(1, connectStop))

      val boundaryStart = 1
      val boundaryStop = 6
      val boundary = new RectangularArea(Point(1, boundaryStart), Point(2, boundaryStop))

      When("computing connection boundaries with a deviation of 1")
      val deviation = 1
      val connectionBoundary = new ConnectionBoundary(boundary, deviation)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be 1 and 6")
      coordinates should be ((boundaryStart, boundaryStop))

    }

    it ("should use the boundary coordinates if the connecting area size + deviation exceeds the boundary when both areas have even size") {

      Given("a boundary with its west side having size 10 (1 -> 10) and a connecting area with east size 4 (1 -> 4)")
      val connectingArea = new RectangularArea(Point(0, 1), Point(1, 4))
      val boundary = new RectangularArea(Point(1, 1), Point(2, 10))

      When("computing connection boundaries with a deviation of 99")
      val deviation = 99
      val connectionBoundary = new ConnectionBoundary(boundary, deviation)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be 1 and 10")
      coordinates should be ((1, 10))

    }

    /*
     *
     *  Tests with one area being even and one area being odd
     *
     */

    it ("should compute the boundaries for when the odd-sized connecting area is 1 size smaller than the boundary area to be the start/stop of the boundary") {

      Given("a connecting area with a side of odd size 3 (1 -> 3) and a boundary area with a side of size 4 (1 -> 4)")
      val connectStart = 1
      val connectStop = 3
      val connectingArea = new RectangularArea(Point(connectStart, 1), Point(connectStop, 4))

      val boundaryStart = 1
      val boundaryStop = 4
      val boundary = new RectangularArea(Point(boundaryStart, 4), Point(boundaryStop, 6))

      When("computing connection boundaries")
      val connectionBoundary = new ConnectionBoundary(boundary)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, North)

      Then("the boundary coordinates should be 1 and 3 or 2 and 4")
      assert(coordinates == (1, 3) || coordinates == (2, 4), "The coordinate boundary was " + coordinates)

    }

    it ("should compute the boundaries for when the even-sized connecting area is 1 size smaller than the boundary area to be as close to (0,0) as possible") {

      Given("a connecting area with a side of even size 4 (1 -> 4) and a boundary area with a side of size 5 (1 -> 5)")
      val connectStart = 1
      val connectStop = 4
      val connectingArea = new RectangularArea(Point(connectStart, 1), Point(connectStop, 4))

      val boundaryStart = 1
      val boundaryStop = 5
      val boundary = new RectangularArea(Point(boundaryStart, 4), Point(boundaryStop, 6))

      When("computing connection boundaries with no deviation")
      val connectionBoundary = new ConnectionBoundary(boundary)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, North)

      Then("the boundary coordinates should be 1 and 4 or 2 and 5")
      assert(coordinates == (1, 4) || coordinates == (2, 5), "The coordinate boundary was " + coordinates)

    }

    it ("should use the boundary coordinates if the connecting area size + deviation exceeds the boundary when the connecting area has even size and is smaller than the boundary") {

      Given("a boundary with its west side having size 13 (1 -> 13) and a connecting area with east size 4 (1 -> 4)")
      val connectStart = 1
      val connectStop = 4
      val connectingArea = new RectangularArea(Point(0, connectStart), Point(1, connectStop))

      val boundaryStart = 1
      val boundaryStop = 13
      val boundary = new RectangularArea(Point(1, boundaryStart), Point(2, boundaryStop))

      When("computing connection boundaries with a deviation of 99")
      val deviation = 99
      val connectionBoundary = new ConnectionBoundary(boundary, deviation)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be 1 and 13")
      coordinates should be ((boundaryStart, boundaryStop))

    }

    it ("should use the boundary coordinates if the connecting area size + deviation exceeds the boundary when the connecting area has odd size and is smaller than the boundary") {

      Given("a boundary with its west side having size 12 (1 -> 12) and a connecting area with east size 3 (1 -> 3)")
      val connectStart = 1
      val connectStop = 3
      val connectingArea = new RectangularArea(Point(0, connectStart), Point(1, connectStop))

      val boundaryStart = 1
      val boundaryStop = 12
      val boundary = new RectangularArea(Point(1, boundaryStart), Point(2, boundaryStop))

      When("computing connection boundaries with a deviation of 99")
      val deviation = 99
      val connectionBoundary = new ConnectionBoundary(boundary, deviation)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be 1 and 12")
      coordinates should be ((boundaryStart, boundaryStop))

    }

    it ("should use coordinates closer to (0, 0) when the boundary cannot be evenly divided") {

      Given("a boundary with a western side of length 5")
      val boundary = new RectangularArea(Point(1, 1), Point(5, 5))

      When("computing connection boundaries with an area having an eastern side of size 4 and deviation 0")
      val connectingArea = new RectangularArea(Point(1, 1), Point(1, 4))
      val connectionBoundary = new ConnectionBoundary(boundary, 0)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, West)

      Then("the boundary coordinates should be the top set of (1, 4) rather than (2, 5)")
      coordinates should be ((1, 4))

    }

    it ("should use coordinates closer to (0, 0) when the connecting area cannot be evenly divided") {

      Given("a boundary with a northern side of size 6")
      val boundary = new RectangularArea(Point(0, 5), Point(5, 9))

      When("computing connection boundaries with an area having a southern side of size 3 and deviation 0")
      val connectingArea = new RectangularArea(Point(0, 4), Point(2, 5))
      val connectionBoundary = new ConnectionBoundary(boundary, 0)
      val coordinates = connectionBoundary.computeConnectionBoundary(connectingArea, North)

      Then("the boundary coordinates should be the left set of (1, 3) rather than (2, 4)")
      coordinates should be ((1, 3))

    }

  }
}
