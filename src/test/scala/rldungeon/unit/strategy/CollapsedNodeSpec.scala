package rldungeon.unit.strategy

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.CollapsedNode
import rldungeon.help.RoomVertex

@RunWith(classOf[JUnitRunner])
class CollapsedNodeSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  def fixture = new {
    val room1 = 1
    val room2 = 2
    val room3 = 3
  }

  describe("CollapsedNode") {

    it ("should compare based on its vertex collection") {
      val f = fixture
      import f._
      val node1 = new CollapsedNode(Set(room1, room2))
      val node2 = new CollapsedNode(Set(room1, room2))
      val node3 = new CollapsedNode(Set(room3, room2))
      assert(node1 == node2)
      assert(node3 != node1)
      assert(node1.hashCode == node2.hashCode)
      assert(node3.hashCode != node1.hashCode)
    }

    it ("should check if it contains a subset of vertices") {

      Given("a node representing three vertices")
      val f = fixture
      import f._
      val node = new CollapsedNode(Set(room1, room2, room3))

      When("checking if the node contains a subset of vertices")

      Then("it should return false if the set contains rooms not represented by this node")
      val otherRoom = 99
      node.containsSubset(Set(room2, otherRoom)) should be (false)

      And("it should return true if every node in a set smaller than the represented set is in the node")
      node.containsSubset(Set(room2, room3)) should be (true)

      And("it should return true if the entire set is in the node")
      node.containsSubset(Set(room2, room1, room3)) should be (true)

    }

  }
}
