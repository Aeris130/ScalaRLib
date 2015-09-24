package net.cyndeline.scalarlib.util

import scala.collection.mutable.ListBuffer

/**
 * A mutable circular list that can be traversed in both directions. Keeps track of the current element that is being
 * viewed by the user.
 * @tparam E Type of element stored in the list.
 */
class DoubleLinkedList[E](elements: E*) {
  private var headElement: Option[Link] = None
  private var currentElement: Link = null
  private var elementAmount = 0

  /* Set up the initial elements in the list. Call next so that elements gets inserted into the same order
   * as submitted. Call next a final time to loop back the current position to the head value.
   */
  for (e <- elements) { add(e); next }
  if (!isEmpty) next // Only call if elements were actually added

  def this(e: E) = this(Seq(e): _*)

  /**
   * @constructor Construct a new empty list.
   * @return A new empty list.
   */
  def this() = this(Seq(): _*)

  /**
   * Moves the pointer one step ahead in the list.
   * @return The element located at the next position in the list.
   */
  def next: E = {
    elementsExist()
    currentElement = currentElement.next
    currentElement.value
  }

  /**
   * Moves the pointer one step back in the list.
   * @return The element located at the previous position in the list.
   */
  def previous: E = {
    elementsExist()
    currentElement = currentElement.previous
    currentElement.value
  }

  /**
   * @return The next element in the list.
   */
  def getNext: E = currentElement.next.value

  /**
   * @return @return The previous element in the list.
   */
  def getPrevious: E = currentElement.previous.value

  /**
   * Moves the pointer to a specific element by traversing the list forward.
   * @param e Element to move to.
   */
  def moveTo(e: E): Unit = moveTo(_ == e)

  /**
   * Moves the pointer to an element matching a function.
   * @param f Function checking for matches.
   */
  def moveTo(f: E => Boolean): Unit = {
    if (f(value))
      return

    var n = next
    var i = 0
    while (!f(n)) {
      if (i >= elementAmount)
        throw new NoSuchElementException("No matching element was found.")
      else {
        n = next
        i += 1
      }
    }
  }

  /**
   * Resets the pointer to the current head element in the list.
   * @return The element currently at the head position.
   */
  def reset: E = {
    currentElement = headElement.getOrElse {
      throw new NoSuchElementException("Cannot set head element, no elements found.")
    }

    currentElement.value
  }

  /**
   * @return the value that the pointer currently points at.
   */
  def value: E = {
    elementsExist()
    currentElement.value
  }

  /**
   * @return The number of elements currently in the list.
   */
  def size: Int = elementAmount

  /**
   * @return True if no elements are present in the list, otherwise false.
   */
  def isEmpty: Boolean = !(size > 0)

  /**
   * Adds an element to this list after the current element. If no element exist in the list, the new element is
   * added as the head entry.
   * @param element Element to add.
   * @return A reference to this list instance, allows chained method calls.
   */
  def add(element: E): DoubleLinkedList[E] = {
    val newEntry = new Link(element)

    if (headElement.isEmpty) {
      newEntry.next = newEntry
      newEntry.previous = newEntry
      headElement = Some(newEntry)
      currentElement = newEntry

    } else {
      val currentValue = currentElement
      val nextValue = currentValue.next

      currentValue.next = newEntry
      nextValue.previous = newEntry
      newEntry.next = nextValue
      newEntry.previous = currentValue
    }

    elementAmount += 1
    this
  }

  /**
   * Removes the value currently pointed at by the list. If the element is the head entry, the next element will take
   * its place.
   * @return A reference to this list instance, allows chained method calls.
   */
  def remove(): DoubleLinkedList[E] = {
    elementsExist()
    if (size == 1) {
      clear()
    } else {
      val nextElement = currentElement.next
      val previousElement = currentElement.previous
      nextElement.previous = previousElement
      previousElement.next = nextElement

      if (currentElement == headElement.get)
        headElement = Some(nextElement)

      currentElement = nextElement
      elementAmount -= 1
    }

    this
  }

  /**
   * Removes all entries in the list.
   */
  def clear(): Unit = {
    headElement = None
    currentElement = null
    elementAmount = 0
  }

  /**
   * Switches every previous/next reference in the list. The current value stays selected.
   */
  def reverse(): Unit = {
    if (!isEmpty) {
      var current = currentElement
      var i = 0
      while (i < size) {
        val tmp = current.next
        current.next = current.previous
        current.previous = tmp
        current = current.next
        i += 1
      }
    }

  }

  /**
   * Traverses the list and finds a matching element.
   * @param f Function that evaluates if an element matches or not.
   * @return The first matching element, or None if no elements exist.
   */
  def find(f: E => Boolean): Option[E] = {
    if (isEmpty) return None

    val start = value

    if (f(start))
      return Some(start)

    var n = next
    while (n != start) {
      if (f(n)) {
        moveTo(start)
        return Some(n)
      }


      n = next
    }

    None
  }

  /**
   * Checks if the list contains an element.
   * @param f Function checking for a matching element.
   * @return True if the element exists, otherwise false.
   */
  def exists(f: E => Boolean): Boolean = find(f).isDefined

  def toVector: Vector[E] = {
    if (isEmpty) {
      Vector[E]()
    } else {
      val result = new ListBuffer[E]()
      var current = currentElement
      var i = 0
      while (i < size) {
        result += current.value
        current = current.next
        i += 1
      }

      result.toVector
    }
  }

  override def toString: String = {
    if (isEmpty) {
      "Empty double linked list"
    } else {
      val currentValue = value
      val list = new ListBuffer[E]()
      val head = headElement.get
      var current = head
      do {
        list += current.value
        current = current.next
      } while (current != head)

      "DoubleLList: " + list.mkString(", ") + " | Current pointer: " + currentValue
    }
  }
  private def elementsExist() {
    if (isEmpty)
      throw new NoSuchElementException("No elements found.")
  }

  private class Link(val value: E) {
    var next: Link = null
    var previous: Link = null
  }

}
