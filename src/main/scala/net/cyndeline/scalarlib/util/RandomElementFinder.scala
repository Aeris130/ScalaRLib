package net.cyndeline.scalarlib.util

import scala.util.Random

/**
 * Selects a random element from a set.
 *
 * @constructor Creates a new random element finder.
 */
class RandomElementFinder extends RandomElementFinderInterface {

  def findRandomElement[E](elementSet: Set[E], random: Random): E = {
    if (elementSet.isEmpty)
      throw new IllegalArgumentException("No elements found in the submitted set.")

    val randomElementIndex = random.nextInt(elementSet.size)
    val elements = elementSet.iterator

    var i = 0
    while (elements.hasNext) {
      val e = elements.next()
      if (i == randomElementIndex)
        return e

      i += 1
    }

    throw new Error("Iteration over random elements failed.")
  }
}

/**
 * For mocking.
 */
trait RandomElementFinderInterface {
  def findRandomElement[E](elements: Set[E], random: Random): E
}
