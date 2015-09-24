package util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.util.DoubleLinkedList

@RunWith(classOf[JUnitRunner])
class DoubleLinkedListSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  describe("DoubleLinkedList") {

    it ("should be empty on creations") {

      Given("a new list")
      val list = new DoubleLinkedList()

      When("checking its size")
      val size = list.size

      Then("the size should be 0")
      size should be (0)

      And("the list should be empty")
      list.isEmpty should be (true)

    }

    it ("should add an element") {

      Given("a list")
      val list = new DoubleLinkedList[Int]()

      When("adding an element")
      list.add(1)

      Then("the current value should be 1")
      list.value should be (1)

      And("the size should be 1")
      list.size should be (1)

      And("the list should not be empty")
      list.isEmpty should be (false)

    }

    it ("should add multiple elements") {

      Given("a list")
      val list = new DoubleLinkedList[Int]()

      When("adding three elements")
      list.add(1).add(2).add(3)

      Then("the current value should be 1")
      list.value should be (1)

      And("the size should be 3")
      list.size should be (3)

    }

    it ("should add initial elements") {

      Given("a list that takes four initial values")
      val list = new DoubleLinkedList[Int](1, 2, 3, 4)

      Then("the list size should be 4")
      list.size should be (4)

    }

    it ("should traverse elements") {

      Given("a list that takes four initial values")
      val list = new DoubleLinkedList[Int](1, 2, 3, 4)

      Then("traversing the list should yield 1, 2, 3, 4")
      list.value should be (1)
      list.next should be (2)
      list.value should be (2)
      list.next should be (3)
      list.value should be (3)
      list.next should be (4)
      list.value should be (4)
      list.next should be (1)
      list.value should be (1)

      And("traversing the list backwards should yield 4, 3, 2, 1")
      list.previous should be (4)
      list.value should be (4)
      list.previous should be (3)
      list.value should be (3)
      list.previous should be (2)
      list.value should be (2)
      list.previous should be (1)
      list.value should be (1)

    }

    it ("should retrieve next/previous elements from the list without changing the pointer") {

      Given("a list of three elements")
      val list = new DoubleLinkedList(1, 2, 3, 4)

      When("examining neighbors of the current element (1)")
      Then("the previous element should be 4")
      list.getPrevious should be (4)
      list.value should be (1)

      And("the next element should be 2")
      list.getNext should be (2)
      list.value should be (1)

    }

    it ("should delete a lone element") {

      Given("a list with a single value")
      val list = new DoubleLinkedList[Int](1)

      When("deleting the current value")
      list.remove()

      Then("the size should be 0")
      list.size should be (0)

      And("the list should be empty")
      list.isEmpty should be (true)

      And("no elements should exist")
      intercept[NoSuchElementException] {
        list.next
      }
      intercept[NoSuchElementException] {
        list.previous
      }
      intercept[NoSuchElementException] {
        list.value
      }
      intercept[NoSuchElementException] {
        list.reset
      }
    }

    it ("should delete elements positioned between other elements") {

      Given("a list with four initial values")
      val list = new DoubleLinkedList[Int](1, 2, 3, 4)

      When("selecting pointing at the second element and removing it")
      list.next
      list.remove()

      Then("the remaining list should be (1, 3, 4)")
      listElements(list) should be (Vector(1, 3, 4))

      And("the size should be (3")
      list.size should be (3)

    }

    it ("should set the head entry as the next value when deleting the head") {

      Given("a list with 2 values")
      val list = new DoubleLinkedList[Int](1, 2)

      When("deleting the head (current value)")
      list.remove()

      Then("the current value should be 2")
      list.value should be (2)

      And("the current value should be set to 2 when resetting")
      list.reset
      list.value should be (2)

      And("the list should have size 1")
      list.size should be (1)

    }

    it ("should update links when deleting an element") {

      Given("a list with four initial values")
      val list = new DoubleLinkedList[Int](1, 2, 3, 4)

      When("deleting value 3")
      list.next
      list.next
      list.remove()

      Then("it should be possible to move between 2 and 4")
      list.previous should be (2)
      list.next should be (4)

    }

    it ("should reset the current value") {

      Given("a list with head entry 1, pointing beyond the head entry")
      val list = new DoubleLinkedList[Int](1, 2, 3, 4)
      list.next
      list.next // 3

      When("resetting the list")
      list.reset

      Then("the current value should be 1")
      list.value should be (1)

    }

    it ("should clear the list") {

      Given("a list with 4 elements")
      val list = new DoubleLinkedList[Int](1, 2, 3, 4)

      When("clearing the list")
      list.clear()

      Then("no elements should be found")
      intercept[NoSuchElementException] {
        list.value
      }

      And("the size should be 0")
      list.size should be (0)

      And("the list should be empty")
      list.isEmpty should be (true)

    }

    it ("should find the head value") {

      Given("a list with the value 1 as head")
      val list = new DoubleLinkedList(1, 2, 3, 4)

      When("finding the value 1")
      val found = list.find(_ == 1)

      Then("the result should be 1")
      found should be (Some(1))

    }

    it ("should find values in front of head") {

      Given("a list with the value 3")
      val list = new DoubleLinkedList(1, 2, 3, 4)

      When("finding the value 3")
      val found = list.find(_ == 3)

      Then("the result should be 3")
      found should be (Some(3))

    }

    it ("should return None if no value is found in a list with elements") {

      Given("a list without the value 5")
      val list = new DoubleLinkedList(1, 2, 3, 4)

      When("finding the value 5")
      val found = list.find(_ == 5)

      Then("the result should be None")
      found should be (None)

    }

    it ("should return None when finding an element in an empty list") {

      Given("an empty list")
      val list = new DoubleLinkedList()

      When("finding a value")
      val found = list.find(_ == 2)

      Then("the result should be None")
      found should be (None)

    }

    it ("should reset the pointer to the current value after searching for something") {

      Given("a list with 5 values, and the pointer on the first")
      val list = new DoubleLinkedList(1, 2, 3, 4, 5)
      list.moveTo(1)

      When("finding the 4th value")
      list.find(_ == 4)

      Then("the current value should be 1")
      list.value should be (1)

    }

    it ("should check if elements exist") {

      Given("a list with values 1, 2, 3, 4")
      val list = new DoubleLinkedList(1, 2, 3, 4)

      When("checking for existence of elements")
      Then("the value 4 should exist")
      list.exists(_ == 4) should be (true)

      And("the value 5 should not exist")
      list.exists(_ == 5) should be (false)

    }

    it ("should move to values") {

      /* This test checks bth moveTo(e) and moveTo(f: E => Boolean) */

      Given("a list with values 1, 2, 3, 4, pointing at the value 1")
      val list = new DoubleLinkedList(1, 2, 3, 4)

      When("moving the 4")
      list.moveTo(4)

      Then("the list should point to 4")
      list.value should be (4)

      And("moving the list to 2 should loop the list to 2")
      list.moveTo(2)
      list.value should be (2)

    }

    it ("should move to a value when a value identical to the start is present in the list") {

      Given("a list with the values 6, 2, 6, 1")
      val list = new DoubleLinkedList(6, 2, 6, 1)

      When("moving to the last value")
      list.moveTo(1)

      Then("the current value should be 1")
      list.value should be (1)

    }

    it ("should parse the dll into an immutable list based on the first position") {

      Given("a double linked list positioned at the head element")
      val list = new DoubleLinkedList(6, 2, 6, 1)

      When("computing an immutable list")
      val iList = list.toVector

      Then("the list should be 6, 2, 6, 1")
      iList should be (Vector(6, 2, 6, 1))

    }

    it ("should parse the dll into an immutable list based on the current position") {

      Given("a double linked list positioned at element 7")
      val list = new DoubleLinkedList(6, 2, 6, 1, 7, 6, 5, 9)
      list.moveTo(7)

      When("computing an immutable list")
      val iList = list.toVector

      Then("the list should be 7, 6, 5, 9, 6, 2, 6, 1")
      iList should be (Vector(7, 6, 5, 9, 6, 2, 6, 1))

    }

    it ("should reverse a list with no elements") {

      Given("an empty list")
      val list = new DoubleLinkedList()

      When("reversing the list")
      list.reverse()

      Then("the new list should be empty")
      assert(list.isEmpty)

    }

    it ("should reverse a list with a single element") {

      Given("a list with a single element")
      val list = new DoubleLinkedList(1)

      When("reversing the list")
      list.reverse()

      Then("the list should only contain a single element")
      list.size should be (1)

      And("it should be 1")
      list.value should be (1)

      And("the previous and next value should be 1")
      list.getNext should be (1)
      list.getPrevious should be (1)

    }

    it ("should reverse a list with multiple elements") {

      Given("a list with multiple elements")
      val list = new DoubleLinkedList(6, 2, 6, 1)
      val original = list.toVector

      When("reversing the list")
      list.reverse()

      Then("the list should be 6, 1, 6, 2")
      list.toVector should be (Vector(6, 1, 6, 2))

      And("moving previous from the current element should give the original list")
      list.reverse()
      list.toVector should equal (original)

    }

  }

  private def listElements(list: DoubleLinkedList[Int]): Vector[Int] = {
    list.reset
    (for (i <- 0 until list.size) yield {
      val v = list.value
      list.next
      v
    }).toVector
  }
}
