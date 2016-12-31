package rldrawing.unit.forceGrid

import helperClasses.RectangleOverlap
import net.cyndeline.rlcommon.math.geom.Dimensions
import net.cyndeline.scalarlib.rldrawing.dungeon.separation.RectanglePlacement
import testHelpers.SpecImports

class RectanglePlacementSpec extends SpecImports {
  private val placer = new RectanglePlacement()

  describe("RectanglePlacement") {

    it ("should place an empty area set") {

      Given("an empty list of areas")
      val areas = Vector[Dimensions]()

      When("placing the areas")
      val placement = placer.computePlacement(areas)

      Then("an empty placement should be produced")
      placement should be (Some(Vector()))

    }

    it ("should place a single area") {

      Given("given a single 3x3 area")
      val areas = Vector(Dimensions(3, 3))

      When("placing the area")
      val placement = placer.computePlacement(areas)

      Then("the area should be placed")
      placement should be ('defined)

      And("a 3x3 rectangle should be produced")
      placement.get should have size 1
      placement.get.head.width should be (3)
      placement.get.head.height should be (3)

    }

    it ("should place multiple areas") {

      Given("three areas with different dimensions")
      val areas = Vector(Dimensions(3, 3), Dimensions(5, 6), Dimensions(4, 4))

      When("placing the area")
      val placement = placer.computePlacement(areas)

      Then("the area should be placed")
      placement should be ('defined)

      And("no area should overlap")
      RectangleOverlap.overlaps(placement.get) should be (false)

    }

  }

}
