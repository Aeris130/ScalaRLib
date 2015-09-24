package net.cyndeline.scalarlib.util

import scala.collection.mutable.ListBuffer

/**
 * Groups a list into n sublists such that the initial sublists contains the remainder needed to keep the amount to n.
 *
 * Example: Given a list of size 6, n = 4 would result in List(1,2), List(3,4), List(5), List(6).
 */
object FrontInterval {

  def apply[E](list: Vector[E], n: Int): Vector[Vector[E]] = {
    require(n > 0)
    val elementsPerSet = list.size / n
    val initialGrouping = list.grouped(elementsPerSet).toVector // note: Int division = floor
    val groups = initialGrouping.size

    if (groups > n) {

      /* There are two cases to consider: If elements per set is 1, then there will be multiple extra groups
       * each containing a single element. Each of those extra groups should have its element assigned to one
       * of the regular groups. Otherwise, there will be a single extra group with more than 1 element in it.
       * This group will contain fewer elements than the amount of sublists.
       *
       * In both cases, 1 extra element will be assigned to each sublist until no more extra elements exist.
       */
      val extraGroups = groups - n
      val elementsPerExtraGroup = initialGrouping.last.size
      val totalNumberOfExtraElements = extraGroups * elementsPerExtraGroup

      // the first n groups will receive an additional element, where n = extraGroups
      val result = new ListBuffer[Vector[E]]()
      val allElements = list.iterator

      var elementsLeftToAdd = totalNumberOfExtraElements
      var groupsLeft = n

      while (groupsLeft > 0) {
        var i = elementsPerSet
        val sublist = new ListBuffer[E]()

        while (i > 0) {
          sublist += allElements.next()
          i -= 1
        }

        if (elementsLeftToAdd > 0) {
          sublist += allElements.next()
          elementsLeftToAdd -= 1
        }

        result += sublist.toVector
        groupsLeft -= 1
      }

      result.toVector
    } else {
      initialGrouping
    }
  }
}
