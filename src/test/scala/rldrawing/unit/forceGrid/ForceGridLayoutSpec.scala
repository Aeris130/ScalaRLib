package rldrawing.unit.forceGrid

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}
import net.cyndeline.scalarlib.rldrawing.ForceGridLayout
import testHelpers.SpecImports

class ForceGridLayoutSpec extends SpecImports {

  describe("ForceGridLayout") {

    it ("should generate openings between adjacent areas") {

      // Need to implement constructing openings on the fly first

      Given("two rectangles sharing a border of length 5")
      val r1 = Rectangle(Point(0, 0), 5, 5)
      val r2 = Rectangle(Point(4, 0), 5, 5)

      When("building a layout")
      val layout = ForceGridLayout(Vector(r1, r2))

      Then("a single opening should be created")
      val openings1 = layout.openingsFor(layout.rooms(0))
      val openings2 = layout.openingsFor(layout.rooms(1))
      openings1 should have size 1
      openings2 should have size 1

      And("the opening should be between (4,1) and (4,3)")
      openings1.head.openings should be (Vector((Point(4, 1), Point(4, 3))))

    }

  }

}
