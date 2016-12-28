package rldrawing.unit.forceGrid

import net.cyndeline.rlcommon.math.geom.{Dimensions, Rectangle}
import net.cyndeline.scalarlib.rldrawing.forceGrid.separation.RectangleSeparation
import testHelpers.SpecImports

class RectangleSeparationSpec extends SpecImports {
  private val algorithm = new RectangleSeparation()
  private val seed = 1234

  describe("RectangleSeparation") {

    it ("should do nothing with an empty rectangle vector") {

      Given("an empty vector of rectangle dimensions")
      val v = Vector[Dimensions]()

      When("separating the vector")
      val separate = algorithm.separate(v, seed)

      Then("the result should be empty")
      separate should be ('empty)

    }

    it ("should process a single rectangle") {

      Given("a vector containing dimensions for a single rectangle")
      val d = Dimensions(3, 4)
      val v = Vector(d)

      When("separating the vector")
      val separate = algorithm.separate(v, seed)

      Then("a single rectangle with the specified dimensions should be computed")
      separate should have size 1
      separate.head should equal (Rectangle(separate.head.start, d))

    }

    it ("should separate multiple rectangles") {

      Given("a vector of rectangles")
      val d1, d2, d3, d4, d5 = Dimensions(5, 5)
      val v = Vector(d1, d2, d3, d4, d5)

      When("separating the vector")
      val separate = algorithm.separate(v, seed)

      Then("the resulting rectangles should at most share an edge")
      separate should have size 5
      rectanglesAreSeparated(separate)

    }

  }

  private def rectanglesAreSeparated(r: Vector[Rectangle]): Unit = {
    var i = 0
    while (i < r.size) {
      var j = i + 1
      while (j < r.size) {
        val overlap = r(i).intersection(r(j))
        assert(overlap.isEmpty || overlap.get.height == 1 || overlap.get.width == 1, s"Rectangles ${r(i)} and ${r(j)} overlap.")
        j += 1
      }
      i += 1
    }
  }
}
