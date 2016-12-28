package rldrawing.unit.continent

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldrawing.MapLayout
import net.cyndeline.scalarlib.rldrawing.common.RRoom
import net.cyndeline.scalarlib.rldrawing.continent.{ContinentFactory, ContinentSettings}
import rldrawing.help.MapLayoutValidation
import testHelpers.SpecImports

class ContinentFactorySpec extends SpecImports {
  private val seed = 999
  private val noWater = 0
  private val someWater = 0.5
  private val allWater = 1

  private def buildSquareFactory(waterMass: Double) = {
    new ContinentFactory(ContinentSettings(waterMass, 0.01, 0.5, 16, false))
  }

  private def buildRoundFactory(waterMass: Double) = {
    new ContinentFactory(ContinentSettings(waterMass, 0.01, 0.5, 16, true))
  }

  describe("ContinentFactory") {

    /*
     * It's a bit tricky to "test" the appearance of continents, this is mostly to check for array-out-of-bounds etc.
     */

    it ("should generate a continent with a single area of size 3") {

      Given("a continent factory")
      val factory = buildSquareFactory(noWater)
      val size = 3

      When("creating a continent with width and height 3")
      val continent = factory.build(1, 1, seed, size)

      Then("the resulting map should have size 1")
      continent.width should be (size)
      continent.height should be (size)

      And("a single area should be added to it")
      continent.rooms should have size 1
      val area = continent.rooms.head
      area.start should be (Point(0, 0))
      area.width should be (size)
      area.height should be (size)

      And("the layout should be valid")
      MapLayoutValidation.validate(continent)

    }

    it ("should scale coordinate sizes") {

      Given("a continent factory and size 4")
      val factory = buildSquareFactory(noWater)
      val size = 4

      When("creating a continent with width 2 and height 3")
      val continent = factory.build(2, 3, seed, size)

      Then("the map should contain a 4x4 area at (0,0)")
      containsArea(continent, Rectangle(Point(0, 0), size, size))

      And("the map should contain a 4x4 area at (3,0)")
      containsArea(continent, Rectangle(Point(3, 0), size, size))

      And("the map should contain a 4x4 area at (0,3)")
      containsArea(continent, Rectangle(Point(0, 3), size, size))

      And("the map should contain a 4x4 area at (3,3)")
      containsArea(continent, Rectangle(Point(3, 3), size, size))

      And("the map should contain a 4x4 area at (0,6)")
      containsArea(continent, Rectangle(Point(0, 6), size, size))

      And("the map should contain a 4x4 area at (3,6)")
      containsArea(continent, Rectangle(Point(3, 6), size, size))

      And("the layout should be valid")
      MapLayoutValidation.validate(continent)

    }

    it ("should generate maps with both land and water of size 1") {

      Given("a factory with 50/50 chance between land and water")
      val factory = buildSquareFactory(someWater)

      When("creating a continent with size 50x60")
      val continent = factory.build(50, 60, seed)

      Then("less than 100x200 areas should be created")
      assert(continent.rooms.size < (50 * 60), s"${continent.rooms.size} areas were created.")

      And("the layout should be valid")
      MapLayoutValidation.validate(continent)

    }

    it ("should generate maps with both land and water of size 3") {

      Given("a factory with 50/50 chance between land and water")
      val factory = buildSquareFactory(someWater)

      When("creating an island with size 50x60 and area size 3")
      val continent = factory.build(50, 60, seed, 3)

      Then("less than 100x200 areas should be created")
      assert(continent.rooms.size < (50 * 60), s"${continent.rooms.size} areas were created.")

      And("the layout should be valid")
      MapLayoutValidation.validate(continent)

    }

    it ("should generate maps with only water") {

      Given("a factory with no chance for land")
      val factory = buildSquareFactory(allWater)

      When("creating an island with size 50x60 and area size 3")
      val continent = factory.build(50, 60, seed, 3)

      Then("the map should be empty")
      continent.rooms should be ('empty)

    }

    it ("should construct connectivity graphs") {

      Given("a 2x3 map with 6 rooms")
      val factory = buildSquareFactory(noWater)
      val size = 4
      val continent = factory.build(2, 3, seed, size)

      When("generating a connectivity graph")
      val graph = continent.connectionGraph

      Then("7 connections should be made")
      graph.edges should have size 7

      And("the connectivity graph should have nodes from 0 to 5")
      GraphCommons.outerVertices(graph).toSet should be (Set(0, 1, 2, 3, 4, 5))

      And("every connection pair should represent rectangles sharing a border")
      for (e <- GraphCommons.outerEdges(graph)) {
        val a = continent.rooms(e._1)
        val b = continent.rooms(e._2)
        val intersection = a.intersection(b)
        assert(intersection.isDefined && (intersection.get.width > 1 || intersection.get.height > 1))
      }

    }

    it ("should construct maps using the particle mask") {

      Given("a factory using the particle mask")
      val factory = buildRoundFactory(0.4)

      When("constructing the map")
      val continent = factory.build(3, 5, seed)

      Then("the map should be valid")
      MapLayoutValidation.validate(continent)

    }

  }

  private def containsArea(map: MapLayout[RRoom], shape: Rectangle): Unit = {
    assert(map.rooms.exists(r => r.start == shape.start && r.width == shape.width && r.height == shape.height))
  }

}
