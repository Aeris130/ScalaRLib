package rldrawing.unit.forceGrid

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.scalarlib.rldrawing.dungeon.separation.PlacementGrid
import testHelpers.SpecImports

class PlacementGridSpec extends SpecImports {

  def buildGrid(size: Int = 10, side: Int = 5) = PlacementGrid(size, side)

  describe("PlacementGrid") {

    it ("should initiate every cell to the largest possible square, or the length to the grid border") {

      Given("a 10x10 grid with the largest side being 5")
      val size = 10
      val largestSide = 5

      When("initiating the grid")
      val grid = buildGrid(size, largestSide)

      Then("(0,0) should have size 5")
      grid.spaceAt(0, 0) should be (5)

      And("(9,9) should have size 1")
      grid.spaceAt(9, 9) should be (1)

      And("(7,7) should have size 3")
      grid.spaceAt(7, 7) should be (3)

    }

    it ("should update the cells around a rectangle when placing it") {

      Given("a 10x10 grid with the largest side being 5")
      val grid = buildGrid()

      When("placing a 4x3 rectangle at (3,4)")
      grid.place(Rectangle(Point(3, 4), 4, 3))

      Then("the coordinates inside the rectangle should be occupied")
      grid.isOccupied(4, 5) should be (true)
      grid.isOccupied(5, 5) should be (true)

      And("the coordinates on the sides closest to (0,0) should have size 1")
      grid.spaceAt(3, 5) should be (1)
      grid.spaceAt(3, 4) should be (1)
      grid.spaceAt(4, 4) should be (1)
      grid.spaceAt(5, 4) should be (1)

      And("the coordinates visible from (9,9) should have size 4")
      grid.spaceAt(3, 6) should be (4)
      grid.spaceAt(4, 6) should be (4)
      grid.spaceAt(5, 6) should be (4)
      grid.spaceAt(6, 6) should be (4)
      grid.spaceAt(6, 4) should be (4)
      grid.spaceAt(6, 5) should be (4)

      And("the cell at (2,2) should have size 3")
      grid.spaceAt(2, 2) should be (3)

      And("the cell at (1,5) should have size 3")
      grid.spaceAt(1, 5) should be (3)

    }

    it ("should updated cell values into consideration when placing multiple areas") {

      Given("a 10x10 grid with the largest side being 3")
      val grid = buildGrid(10, 3)

      When("placing a 3x3 area at (5,5) and a 3x3 area at (7,5)")
      grid.place(Rectangle(Point(5, 5), 3, 3))
      grid.place(Rectangle(Point(7, 5), 3, 3))

      Then("the cell at (5,5) should have 1 space avilable from when the first area was placed")
      grid.spaceAt(5, 5) should be (1)

    }

    it ("should confirm that a rectangle fits next to another rectangle despite the required space being partitioned across multiple cells") {

      Given("a 10x10 grid with the largest side being 5 and a 3x6 area at (4,1)")
      val grid = buildGrid()
      grid.place(Rectangle(Point(4, 1), 3, 6))

      When("checking if a 3x8 area can be placed at (2,0)")
      val canBePlaced = grid.canBePlaced(Dimensions(3, 8), 2, 0)

      Then("the result should be true")
      canBePlaced should be (true)

    }

    it ("should deny that a rectangle fits at a cell if placing the rectangle there causes it to go out of bounds") {

      Given("a 10x10 grid")
      val grid = buildGrid()

      When("checking if a 5x5 rectangle fits at (8,3)")
      val canBePlaced = grid.canBePlaced(Dimensions(5, 5), 8, 3)

      Then("the result should be false")
      canBePlaced should be (false)

    }

    it ("should deny that a rectangle fits in a cell if the cell has enough space to contain one axis, but the other" +
        "axis causes the rectangle to go out of bounds") {

      Given("a 10x10 grid")
      val grid = buildGrid()

      When("checking if a 3x4 area fits at (7,7)")
      val canBePlaced = grid.canBePlaced(Dimensions(3, 4), 7, 7)

      Then("the result should be false")
      canBePlaced should be (false)

    }

    it ("should deny that a rectangle fits in a cell if the cell has enough space to contain one axis, but the other" +
      "axis causes the rectangle to overlap another area") {

      Given("a 10x10 grid with a 4x4 area at (6,6)")
      val grid = buildGrid(10, 4)
      grid.place(Rectangle(Point(6, 6), 4, 4))

      When("checking if a 3x4 area can be placed at (6,4)")
      val canBePlaced = grid.canBePlaced(Dimensions(3, 4), 6, 4)

      Then("the result should be false")
      canBePlaced should be (false)

    }

    it ("should place a rectangle that spans multiple cell-squares") {

      Given("a 10x10 grid with the largest side being 5 and a 3x6 area at (4,1)")
      val grid = buildGrid()
      grid.place(Rectangle(Point(4, 1), 3, 6))

      When("checking if a 3x8 area can be placed at (2,0)")
      grid.place(Rectangle(Point(2, 0), 3, 8))

      Then("(3,1) to (3,6) should be occupied")
      for (i <- 1 to 6)
        assert(grid.isOccupied(3, i), s"(3,$i) was not occupied.")

      And("(4,1) to (4,5) should have 1 remaining space left")
      for (i <- 1 to 5)
        assert(grid.spaceAt(4, i) == 1, s"(4,$i) has ${grid.spaceAt(4, i)} left, not 1.")

      And("cells to the left of the new rectangles initial square should be updated")
      grid.spaceAt(0, 0) should be (3)
      grid.spaceAt(1, 1) should be (2)
      grid.spaceAt(2, 1) should be (1)

      And("cells to the left of the new rectangles middle square should be updated")
      grid.spaceAt(0, 4) should be (3)

      And("cells to the left of the final strip of non-square cells of the new rectangle should be updated")
      grid.spaceAt(1, 6) should be (2)

      And("cells above and left of the rectangle should be unchanged")
      grid.spaceAt(0, 7) should be (3)

    }

    it ("should throw an exception when attempting to place a rectangle partially outside the grid") {

      Given("a 10x10 grid with the largest side being 5")
      val grid = buildGrid()

      When("placing a 5x4 rectangle at (6,6)")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        grid.place(Rectangle(Point(6, 6), 5, 4))
      }

    }

    it ("should throw an error when attempting to place an area such that it overlaps with another area") {

      Given("a grid with a 4x4 area at (6,6)")
      val grid = buildGrid(10, 4)
      grid.place(Rectangle(Point(6, 6), 4, 4))

      When("placing a 3x4 area at (6,4)")
      Then("an exception should be thrown")
      intercept[Error] {
        grid.place(Rectangle(Point(6, 4), 3, 4))
      }

    }


  }



}
