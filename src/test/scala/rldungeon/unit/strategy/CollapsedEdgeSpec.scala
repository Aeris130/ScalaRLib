package rldungeon.unit.strategy

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import testHelpers.SpecImports

class CollapsedEdgeSpec extends SpecImports {

  def fixture = new {
    val room1 = 1
    val room2 = 2
    val room3 = 3
    val room4 = 4

    val originalTarget1 = 51
    val originalTarget2 = 52

    val node1 = new CollapsedNode(Set(room1))
    val node2 = new CollapsedNode(Set(room2))

    val superNode1 = new CollapsedNode(Set(room1, room2))
    val superNode2 = new CollapsedNode(Set(room3, room4))
  }

  describe("CollapsedEdge") {

    it ("should use its companion object to retrieve its targets represented vertices") {

      Given("an edge targeting two collapsed nodes representing a single room 1, 2 each")
      val f = fixture
      import f._
      val edge = CollapsedEdge(node1, node2)

      When("retrieving targets")
      val targets = CollapsedEdge.targetsOfEdge(edge)

      Then("the result should be (1, 2)")
      targets should be ((room1, room2))

    }

    it ("should use its companion object to retrieve original targets if any exist") {

      Given("an three edges with original targets as its first, second and both targets")
      val f = fixture
      import f._
      val firstOriginal = CollapsedEdge(superNode1, node2, Option(originalTarget1), None)
      val secondOriginal = CollapsedEdge(node1, superNode2, None, Option(originalTarget2))
      val bothOriginal = CollapsedEdge(superNode1, superNode2, Option(originalTarget1), Option(originalTarget2))

      When("retrieving targets")
      val firstTargets = CollapsedEdge.targetsOfEdge(firstOriginal)
      val secondTargets = CollapsedEdge.targetsOfEdge(secondOriginal)
      val thirdTargets = CollapsedEdge.targetsOfEdge(bothOriginal)

      Then("the first edge targets should be original target 1 and node 2")
      firstTargets should be ((originalTarget1, room2))

      And("the second edge targets should be node 1 and original target 2")
      secondTargets should be ((room1, originalTarget2))

      And("the third edge targets should be original targets 1 and 2")
      thirdTargets should be ((originalTarget1, originalTarget2))

    }

  }
}
