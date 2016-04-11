package rldrawing.unit.orthogonalGridCompaction

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Rectangle
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.util.GridPartition
import testHelpers.SpecImports

class GridPartitionSpec extends SpecImports {

  private case class Element(start: Point, stop: Point) extends Rectangle

  describe("GridPartition") {

    it ("should throw an exception when querying coordinates outside the grid") {

      Given("a grid that goes from 2, 3 to 6, 8")
      val grid = new GridPartition[Element](Point(2, 3), Point(6, 8), 1)

      Then("retrieving elements below 2 on the x axis should throw an error")
      intercept[Error] {
        grid.elementsIn(Point(1, 3), Point(6, 8))
      }

      And("retrieving elements above 6 on the x axis should throw an error")
      intercept[Error] {
        grid.elementsIn(Point(2, 3), Point(7, 8))
      }

      And("retrieving elements below 3 on the 7 axis should throw an error")
      intercept[Error] {
        grid.elementsIn(Point(2, 2), Point(6, 8))
      }

      And("retrieving elements above 8 on the x axis should throw an error")
      intercept[Error] {
        grid.elementsIn(Point(2, 3), Point(6, 9))
      }

    }

    it ("should throw an exception when querying coordinates where the stop value lies closer to (0, 0) than the start value") {

      Given("a grid that goes from 2, 2 to 3, 3")
      val grid = new GridPartition[Element](Point(2, 3), Point(6, 8), 1)

      Then("retrieving elements in the area given by 3, 3 and 2, 2 should throw an exception")
      intercept[Error] {
        grid.elementsIn(Point(3, 3), Point(2, 2))
      }

      And("retrieving elements in the area given by 3, 3 and 3, 2 should throw an exception")
      intercept[Error] {
        grid.elementsIn(Point(3, 3), Point(3, 2))
      }

      And("retrieving elements in the area given by 3, 3 and 2, 3 should throw an exception")
      intercept[Error] {
        grid.elementsIn(Point(3, 3), Point(2, 3))
      }

    }

    it ("should throw an exception if the partition ratio is outside the range x, 0 < x <= 1") {

      Given("a ratio of -1, 0 and 1.1")
      val negativeRatio = -1
      val zeroRatio = 0
      val highRatio = 1.1

      When("creating a new grid")

      Then("the negative ratio should throw an exception")
      intercept[IllegalArgumentException] {
        new GridPartition[Element](1, 1, negativeRatio)
      }

      And("the zero ratio should throw an exception")
      intercept[IllegalArgumentException] {
        new GridPartition[Element](1, 1, zeroRatio)
      }

      And("the high ratio should throw an exception")
      intercept[IllegalArgumentException] {
        new GridPartition[Element](1, 1, highRatio)
      }

    }

    it ("should throw an exception when creating a grid where the start coordinate is higher than the stop coordinate") {

      Given("a point A and a point B where A > B")
      val A = Point(4, 4)
      val B = Point(2, 2)

      When("creating a new grid")
      Then("an exception should be thrown")
      intercept[Error] {
        new GridPartition[Element](A, B, 0.5)
      }

    }

    it ("should throw an exception when adding an element outside the grid coordinates") {

      Given("a grid with coordinates between 3, 3 and 9, 9")
      val grid = new GridPartition[Element](Point(3, 3), Point(9, 9), 1)

      When("adding an element to coordinate 10, 10")
      val e = Element(Point(10, 10), Point(11, 11))

      Then("an error should be thrown")
      intercept[Error] {
        grid.add(e)
      }

    }

    it ("should throw an exception when removing an element outside the grid coordinates") {

      Given("a grid that goes from 2, 3 to 6, 8")
      val grid = new GridPartition[Element](Point(2, 3), Point(6, 8), 1)

      Then("removing elements below 2 on the x axis should throw an error")
      intercept[Error] {
        grid.remove(Element(Point(1, 3), Point(6, 8)))
      }

      And("removing elements above 6 on the x axis should throw an error")
      intercept[Error] {
        grid.remove(Element(Point(2, 3), Point(7, 8)))
      }

      And("removing elements below 3 on the 7 axis should throw an error")
      intercept[Error] {
        grid.remove(Element(Point(2, 2), Point(6, 8)))
      }

      And("removing elements above 8 on the x axis should throw an error")
      intercept[Error] {
        grid.remove(Element(Point(2, 3), Point(6, 9)))
      }

    }

    it ("should create a grid with a single partition") {

      Given("a ratio of 1.0")
      val r = 1.0

      When("creating a grid of size 10")
      val grid = new GridPartition[Element](Point(0, 0), Point(9, 9), r)

      Then("the grid should be of width 1")
      grid.partitionWidth should be (1)

      And("the grid should be of height 1")
      grid.partitionHeight should be (1)

    }

    it ("should create a grid with multiple partitions") {

      Given("a ratio of 0.5")
      val r = 0.5

      When("creating a grid of size 10")
      val grid = new GridPartition[Element](Point(0, 0), Point(9, 9), r)

      Then("the grid should be of width 2")
      grid.partitionWidth should be (2)

      And("the grid should be of height 2")
      grid.partitionHeight should be (2)

    }

    it ("should index values in a single partition") {

      Given("a grid partition with ratio 1.0")
      val grid = new GridPartition[Element](Point(0, 0), Point(9, 9), 1)

      When("adding an element")
      val e = Element(Point(4, 4), Point(6, 6))
      grid.add(e)

      Then("the element should be indexed at all coordinates of the grid")
      for (i <- 0 to 9; j <- 0 to 9) {
        val p = Point(i, j)
        val elements = grid.elementAt(p)
        assert(elements == Set(e), "Element set at " + p + " was " + elements + " instead of " + Set(e))
      }

    }

    it ("should index values at the lowest/highest possible coordinate") {

      Given("a grid from 0,0 to 3,3")
      val grid = new GridPartition[Element](Point(0, 0), Point(3, 3), 0.3)

      When("adding elements to coordinates 0,0 and 3,3")
      val e1 = Element(Point(0, 0), Point(0, 0))
      val e2 = Element(Point(3, 3), Point(3, 3))
      grid.add(e1)
      grid.add(e2)

      Then("those elements should be found at those coordinates")
      grid.elementAt(Point(0, 0)) should be (Set(e1))
      grid.elementAt(Point(3, 3)) should be (Set(e2))

    }

    it ("should index values to multiple partitions if the values coordinates overlap them") {

      Given("a 4x4 grid")
      val grid = new GridPartition[Element](Point(0, 0), Point(3, 3), 0.5)

      When("adding a value to coordinate range 1,1 -> 1,3")
      val e = Element(Point(1, 1), Point(1, 3))
      grid.add(e)

      Then("the value should be found on all coordinates between 1,1 and 1,3")
      grid.elementAt(Point(1, 1)) should be (Set(e))
      grid.elementAt(Point(1, 2)) should be (Set(e))
      grid.elementAt(Point(1, 3)) should be (Set(e))

      And("the value should be found in coordinates sharing the same 2x2 partition")
      grid.elementAt(Point(0, 0)) should be (Set(e))
      grid.elementAt(Point(1, 0)) should be (Set(e))
      grid.elementAt(Point(0, 1)) should be (Set(e))
      grid.elementAt(Point(0, 2)) should be (Set(e))
      grid.elementAt(Point(1, 2)) should be (Set(e))
      grid.elementAt(Point(0, 3)) should be (Set(e))

      And("the value should not be found anywhere else")
      grid.elementAt(Point(2, 0)) should be (Set())
      grid.elementAt(Point(2, 1)) should be (Set())
      grid.elementAt(Point(2, 2)) should be (Set())
      grid.elementAt(Point(2, 3)) should be (Set())
      grid.elementAt(Point(3, 0)) should be (Set())
      grid.elementAt(Point(3, 1)) should be (Set())
      grid.elementAt(Point(3, 2)) should be (Set())
      grid.elementAt(Point(3, 3)) should be (Set())

    }

    it ("should index values to every partition if the value covers the entire grid") {

      Given("a 4x4 grid")
      val grid = new GridPartition[Element](Point(0, 0), Point(3, 3), 0.5)

      When("adding an element that covers the grid")
      val e = Element(Point(0, 0), Point(3, 3))
      grid.add(e)

      Then("every coordinate on the grid should have the element")
      grid.elementAt(Point(0, 0)) should be (Set(e))
      grid.elementAt(Point(0, 1)) should be (Set(e))
      grid.elementAt(Point(0, 2)) should be (Set(e))
      grid.elementAt(Point(1, 1)) should be (Set(e))
      grid.elementAt(Point(1, 2)) should be (Set(e))
      grid.elementAt(Point(1, 3)) should be (Set(e))
      grid.elementAt(Point(2, 0)) should be (Set(e))
      grid.elementAt(Point(2, 1)) should be (Set(e))
      grid.elementAt(Point(2, 2)) should be (Set(e))
      grid.elementAt(Point(2, 3)) should be (Set(e))
      grid.elementAt(Point(3, 0)) should be (Set(e))
      grid.elementAt(Point(3, 1)) should be (Set(e))
      grid.elementAt(Point(3, 2)) should be (Set(e))
      grid.elementAt(Point(3, 3)) should be (Set(e))

    }

    it ("should retrieve values in a single partition when there are multiples") {

      Given("a grid with four partitions of size 2")
      val grid = new GridPartition[Element](8, 8, 0.5)

      When("adding an element to the partition going from (0, 0) to (3, 3)")
      val e = Element(Point(1, 1), Point(2, 2))
      grid.add(e)

      Then("the element should be indexed to coordinates inside its partition")
      grid.elementsIn(Point(2, 2), Point(3, 3)) should be (Set(e))

      And("the element should be indexed to coordinates that lies both inside and outside of the partition")
      grid.elementsIn(Point(2, 2), Point(5, 5)) should be (Set(e))

    }

    it ("should retrieve values from a partition when specifying a 1-dimensional set of coordinates") {

      Given("a grid partition with size 4 with partitions of size 2")
      val grid = new GridPartition[Element](4, 4, 0.5)

      When("adding an element to the single point (1, 1)")
      val e = Element(Point(1, 1), Point(1, 1))
      grid.add(e)

      Then("querying the range of coordinates from (1, 0) to (1, 2) should yield the element")
      grid.elementsIn(Point(1, 0), Point(1, 2)) should be (Set(e))

      And("querying the single coordinate (1, 1) should yield the element")
      grid.elementAt(Point(1, 1)) should be (Set(e))

    }

    it ("should remove an element based on its coordinates") {

      Given("an element with area between (2, 3) and (4, 5) in a grid with an additional element between (6,7) and (8,9)")
      val grid = new GridPartition[Element](10, 10, 0.5)
      val e1 = Element(Point(2, 3), Point(4, 5))
      val e2 = Element(Point(6, 7), Point(8, 9))
      grid.add(e1)
      grid.add(e2)

      When("removing element 1 from the grid")
      grid.remove(e1)

      Then("no element should exist in the upper left partition between (2, 3) and (4, 5)")
      grid.elementsIn(Point(2, 3), Point(4, 5)) should be (Set())

      And("element 2 should still be in the grid")
      grid.elementsIn(Point(6, 7), Point(8, 9)) should be (Set(e2))

    }

    it ("should select partition size based on even divisors") {

      Given("a grid of width 8 and height 4 with a 1/2 partition ratio")
      val grid = new GridPartition[Element](8, 4, 0.5)

      Then("the partition width should be 2")
      grid.partitionWidth should be (2)

      And("the partition height should be 2")
      grid.partitionHeight should be (2)

    }

  }
}
