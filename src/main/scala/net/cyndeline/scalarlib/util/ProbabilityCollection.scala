package net.cyndeline.scalarlib.util

import scala.util.Random
import java.util
import scala.collection.JavaConversions._

/**
 * Maps weights towards objects, making it possible to increase the rate at which some objects are
 * returned compared to others.
 *
 * To make an item more likely to be returned, assign it a greater height. The probability of a single element
 * to be selected is the elements weight divided by the total weight in the collection.
 *
 * Source: http://stackoverflow.com/a/6409791
 *
 * @constructor Creates a new probability collection.
 * @param values All initial elements and probability weights (must be higher than 0).
 * @tparam E The type of object to assign weights to.
 */
class ProbabilityCollection[E] private (random: Random, values: Vector[(Double, E)]) extends RandomCollection[E] {
  private var weightAndElements: util.NavigableMap[Double, E] = new util.TreeMap[Double, E]()
  private var totalWeight: Double = 0

  /* Stores all values so that they can be sent to future collections. When the treemap produces its iterator, it
   * uses the weights as they have been added into the map. I.e, the weights 1.0 and 2.0 becomes 1.0 and 1.0+2.0 etc.
   *
   * DO NOT MODIFY THIS DIRECTLY, use the helper method.
   */
  private var originalValues: Vector[(Double, E)] = Vector[(Double, E)]()

  val weightAndValues = values.iterator
  while (weightAndValues.hasNext) {
    val weightAndValue = weightAndValues.next()
    add(weightAndValue._1, weightAndValue._2)
  }

  /**
   * Creates an empty probability collection.
   * @param random Random object used when selecting elements.
   */
  def this(random: Random) = {
    this(random, Vector[(Double, E)]())
  }

  /**
   * Creates a new collection with an initial set of elements and probabilities.
   * @param random Random object used when selecting elements.
   * @param valuesSet All initial elements and probability weights.
   */
  def this(random: Random, valuesSet: (Double, E)*) = {
    this(random, valuesSet.toVector)
  }

  /**
   * Add an item and assign a weight to it.
   *
   * @param weight Weight to assign to the item. Must be higher than 0.
   * @param element Item to map height to.
   */
  override def add(weight: Double, element: E) {
    if (weight > 0) {
      totalWeight += weight
      weightAndElements.put(totalWeight, element)
      addOriginalValue(weight, element)
    } else {
      throw new Error("Only positive probability weights can be used for RandomCollections, element " + element + " has weight " + weight)
    }
  }

  /**
   * Adds elements and weights of another collection to this one.
   * @param collection Collection to add.
   */
  override def addCollection(collection: RandomCollection[E]) {
    val elements = collection.iterator
    while (elements.hasNext) {
      val e: (Double, E) = elements.next()
      add(e._1, e._2)
    }
  }

  /**
   * Removes an element from the collection by creating a new map from the set of old values
   * minus the one to be removed.
   * @param element Element to remove.
   */
  override def remove(element: E) {
    val elements = originalValues.iterator
    originalValues = Vector[(Double, E)]()
    totalWeight = 0
    weightAndElements = new util.TreeMap()

    while (elements.hasNext) {
      val e: (Double, E) = elements.next()
      if (e._2 != element)
        add(e._1, e._2)
    }
  }

  /**
   * Fetches an item from the collection at random. items with higher
   * weight will be more likely to be returned.
   *
   * @return A random item with weighted probability that skews towards
   * 			greater weights.
   */
  override def next: E = {
    if (weightAndElements.isEmpty)
      throw new NoSuchElementException("next on an empty random collection")

    val value = random.nextDouble * totalWeight
    weightAndElements.ceilingEntry(value).getValue
  }

  /**
   * @return the size of the collection.
   */
  override def size: Int = weightAndElements.size

  /**
   * @return True if the collection contains no elements, otherwise false.
   */
  override def isEmpty: Boolean = size == 0

  /**
   * Returns each element stored without weights.
   * @return A list containing one copy of each element in no
   * 			particular order.
   */
  override def allElements: Vector[E] = weightAndElements.map(weightAndElement => weightAndElement._2).toVector

  /**
   * Iterates over every weight and element in the collection.
   * @return An iterator over the collection.
   */
  override def iterator: Iterator[(Double, E)] = originalValues.iterator

  /**
   * Creates a copy of this collection using the same Random object.
   * @return A new collection with the same weights and elements.
   */
  override def copy: RandomCollection[E] = newCollection(random)

  /**
   * Builds a collection using a new random object and the same values as the old collection.
   * @param random Random object used when selecting elements.
   * @return A RandomCollection with a new Random object and the same elements and weights as the old collection.
   */
  override def newCollection(random: Random): RandomCollection[E] = new ProbabilityCollection[E](random, originalValues)

  /**
   * Used for testing to inspect values inside the tree map.
   * @return Returns a list of tuples where the weight is the combined weight as found in the map. Example: Three
   *         elements with weightd 1, 2 and 3 will have weights 1, 3 and 6 inside the map.
   */
  def combinedWeights: Vector[(Double, E)] = weightAndElements.iterator.toVector

  private def addOriginalValue(d: Double, e: E) {
    originalValues = ((d, e) +: originalValues).sortWith((v1: (Double, E), v2: (Double, E)) => v1._1 < v2._1)
  }
}
