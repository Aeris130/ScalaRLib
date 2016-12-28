package rldrawing.unit.util.noise

import net.cyndeline.scalarlib.rldrawing.util.noise.Simplex2D
import testHelpers.SpecImports

import scala.util.Random

class Simplex2DSpec extends SpecImports {

  describe("Simplex2D") {

    it ("should generate a value between -1 and 1") {

      Given("a set of x/y coordinates and a simplex generator")
      val seed = 999
      val r = new Random(seed)
      val coordinates = for (_ <- 0 until 10000) yield (r.nextInt(), r.nextInt())
      val simplex = Simplex2D(seed)

      When("generating simplex noise for each coordinate")
      val noise = coordinates.map(c => simplex.noise(c._1, c._2))

      Then("every value should be between -1 and 1")
      assert(!noise.exists(v => v < 0 || v > 1))

    }

  }

}
