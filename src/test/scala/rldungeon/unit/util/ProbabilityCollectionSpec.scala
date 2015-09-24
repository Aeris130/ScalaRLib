package rldungeon.unit.util

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import net.cyndeline.scalarlib.util.ProbabilityCollection

@RunWith(classOf[JUnitRunner])
class ProbabilityCollectionSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  describe("ProbabilityCollection") {

    it ("should return the same values from objects with the same seed") {

      Given("three random collections with the same content and seeds")
      val r1 = new Random(1)
      val collection1 = new ProbabilityCollection[Int](r1)
      val r2 = new Random(1)
      val collection2 = new ProbabilityCollection[Int](r2)
      val r3 = new Random(1)
      val collection3 = new ProbabilityCollection[Int](r3)

      When("adding 5 elements and retriving 1 element from each collection")
      collection1.add(1, 1)
      collection1.add(1, 2)
      collection1.add(1, 3)
      collection1.add(1, 4)
      collection1.add(1, 5)
      collection2.add(1, 1)
      collection2.add(1, 2)
      collection2.add(1, 3)
      collection2.add(1, 4)
      collection2.add(1, 5)
      collection3.add(1, 1)
      collection3.add(1, 2)
      collection3.add(1, 3)
      collection3.add(1, 4)
      collection3.add(1, 5)

      val result1 = collection1.next
      val result2 = collection2.next
      val result3 = collection3.next

      Then("all results should be equal")
      result1 should equal (result2)
      result2 should equal (result3)

    }

    it ("should throw an exception if retrieving a next() value on an empty collection") {

      Given("an empty collection")
      val collection = new ProbabilityCollection[Int](new Random())

      When("retrieving an element")
      Then("an exception should be thrown")
      intercept[NoSuchElementException] {
        collection.next
      }

    }

    it ("should throw an error when attempting to add elements with probability 0") {

      Given("a probability collection")
      val collection = new ProbabilityCollection[String](new Random())

      When("adding an element with probability 0")
      Then("an error should be thrown")
      intercept[Error] {
        collection.add(0, "")
      }

    }

    it ("should throw an error when attempting to add elements with negative probability") {

      Given("a probability collection")
      val collection = new ProbabilityCollection[String](new Random())

      When("adding an element with probability -1")
      Then("an error should be thrown")
      intercept[Error] {
        collection.add(-1, "")
      }

    }

    it ("should allow creation of an empty collection") {

      When("creating an empty collection")
      val collection = new ProbabilityCollection[String](new Random())

      Then("the collection should be empty")
      collection.size should be (0)
      collection.isEmpty should be (true)
      collection.allElements should be ('empty)

    }

    it ("should allow creation from a sequence of tuples") {

      Given("a list of weight/element tuples")
      val list = Vector((1.0, "A"), (2.0, "B"))

      When("creating a collection")
      val collection = new ProbabilityCollection[String](new Random(), list:_*)

      Then("both tuples should be in the collection")
      collection.iterator.toVector should equal (list)
      collection.allElements should contain("A")
      collection.allElements should contain("B")

    }

    it ("should iterate over the element/weight set") {

      Given("a collection with two elements having weight 1 and 2")
      val collection = new ProbabilityCollection[String](new Random())
      collection.add(1.0, "A")
      collection.add(2.0, "B")

      When("iterating over the elements")
      val elements = collection.iterator.toSet

      Then("both tuples should be present")
      elements should have size (2)
      elements should contain ((1.0, "A"))
      elements should contain ((2.0, "B"))

    }

    it ("should preserve weights and elements when copying the collection") {

      Given("a collection with 3 elements")
      val collection = new ProbabilityCollection[String](new Random())
      collection.add(1.0, "A")
      collection.add(2.0, "B")
      collection.add(3.0, "C")

      When("copying the collection")
      val copy = collection.copy.asInstanceOf[ProbabilityCollection[String]]

      Then("the collection should contain the same elements")
      copy.combinedWeights.toVector should equal (Vector((1.0, "A"), (3.0, "B"), (6.0, "C")))
      copy.iterator.toVector should equal (Vector((1.0, "A"), (2.0, "B"), (3.0, "C")))

    }

    it ("should preserve weights and elements when copying the collection using a new random object") {

      Given("a collection with 3 elements")
      val collection = new ProbabilityCollection[String](new Random())
      collection.add(1.0, "A")
      collection.add(2.0, "B")
      collection.add(3.0, "C")

      When("copying the collection using a new Random object")
      val copy = collection.newCollection(new Random()).asInstanceOf[ProbabilityCollection[String]]

      Then("the collection should contain the same elements")
      copy.combinedWeights.toVector should equal (Vector((1.0, "A"), (3.0, "B"), (6.0, "C")))
      copy.iterator.toVector should equal (Vector((1.0, "A"), (2.0, "B"), (3.0, "C")))

    }

    it ("should allow duplicates") {

      Given("a collection")
      val collection = new ProbabilityCollection[String](new Random())

      When("adding the same element twice")
      collection.add(1.0, "A")
      collection.add(1.0, "A")

      Then("both elements should be in the collection")
      collection.iterator.toVector should equal (Vector((1.0, "A"), (1.0, "A")))
      collection.combinedWeights should equal (Vector((1.0, "A"), (2.0, "A")))

    }

    it ("should preserve the collection when removing entries") {

      Given("a collection with three entries")
      val collection = new ProbabilityCollection[String](new Random())
      collection.add(1.0, "A")
      collection.add(2.0, "B")
      collection.add(3.0, "C")

      When("removing the second entry")
      collection.remove("B")

      Then("two elements should remain")
      collection.size should be (2)
      collection.allElements should contain("A")
      collection.allElements should contain("C")
      collection.iterator.toVector should be (Vector((1.0, "A"), (3.0, "C")))
      collection.combinedWeights should equal (Vector((1.0, "A"), (4.0, "C")))

    }

    it ("should add a collection") {

      Given("two collections with elements in them")
      val collection1 = new ProbabilityCollection[String](new Random())
      collection1.add(1.0, "A")
      collection1.add(2.0, "B")
      collection1.add(3.0, "C")

      val collection2 = new ProbabilityCollection[String](new Random())
      collection2.add(1.5, "D")
      collection2.add(2.5, "E")
      collection2.add(3.5, "F")

      When("adding one collection to the other")
      collection1.addCollection(collection2)

      Then("the first collection should have all elements in their weight order")
      collection1.iterator.toVector should equal (Vector((1.0, "A"), (1.5, "D"), (2.0, "B"), (2.5, "E"), (3.0, "C"), (3.5, "F")))
      collection1.combinedWeights should equal (Vector((1.0, "A"), (3.0, "B"), (6.0, "C"), (7.5, "D"), (10.0, "E"), (13.5, "F")))

    }

    it ("should do nothing when adding an empty collection") {

      Given("a collection with elements and an empty collection")
      val collection1 = new ProbabilityCollection[String](new Random())
      collection1.add(1.0, "A")
      collection1.add(2.0, "B")
      collection1.add(3.0, "C")
      val size = collection1.size

      val collection2 = new ProbabilityCollection[String](new Random())

      When("adding the empty collection")
      collection1.addCollection(collection2)

      Then("the first collection should remain as-is")
      collection1.size should be (size)
      collection1.iterator.toVector should be (Vector((1.0, "A"), (2.0, "B"), (3.0, "C")))
      collection1.combinedWeights should be (Vector((1.0, "A"), (3.0, "B"), (6.0, "C")))

    }

    /**
     * Test checking that an element is returned at the expected rate.
     */
    it ("should return elements at a rate corresponding to their weight") {

      Given("a collection with total weight 20, and an element C with weight 3")
      val collection1 = new ProbabilityCollection[String](new Random())
      collection1.add(8.0, "A")
      collection1.add(9.0, "B")
      collection1.add(3.0, "C")

      /* Perform some modifications first to check if they affect the result. */
      val collectionToTest = collection1.copy
      collectionToTest.remove("C")
      val dummyCollection = new ProbabilityCollection[String](new Random(), (3.0, "C"))
      collectionToTest.addCollection(dummyCollection)

      When("retrieving a large amount of values")
      val runs = 10000000
      val precision = 0.05 * runs // 5% error margin with 10000000 runs

      var cFound = 0
      for (i <- 0 to runs) if (collectionToTest.next == "C") cFound += 1

      Then("the element C should make up roughly 3/20 parts of the result")
      val expectedOccurences = (3.0/20) * runs
      assert((cFound - expectedOccurences) < precision, "The number of element occurences (" + cFound + ") did not match the expected outcome (" + expectedOccurences + "). The allowed error margin was " + precision + ", but the error was " + (cFound - expectedOccurences) + ".")

    }

  }
}
