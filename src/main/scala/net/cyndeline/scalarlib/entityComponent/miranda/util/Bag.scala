package net.cyndeline.scalarlib.entityComponent.miranda.util

/**
 * A collection with constant lookup time based on indices, that doesn't shrink when
 * an element is removed at a particular index. Grows if an element is inserted at an index
 * above the current max size.
 */
class Bag[E] {
  private var elements = Array.fill[Option[E]](64)(null)
  private var bagSize = 0

  def add(item: E) = {
    if (size == capacity) {
      grow()
    }

    elements(size) = Option(item)
    bagSize += 1
  }

  def get(index: Int): Option[E] = {
    if (index >= 0 && index < elements.length) {
      if (elements(index) != null) elements(index) else None
    } else {
      None
    }
  }

  def set(index: Int, item: E) {
    if (index >= capacity) {
      grow(index * 2)
    }

    /* Size shouldn't increase unless an actual component
     * is inserted where there was none.
     */
    if (elements(index) != null && elements(index).isEmpty) {
      bagSize += 1
    }

    elements(index) = Option(item)
  }

  def remove(index: Int): Option[E] = {
    val removed = elements(index)

    if (removed != null && removed.isDefined) {
      elements(index) = None
      bagSize -= 1
      removed
    } else {
      None
    }
  }

  def size: Int = bagSize

  def capacity: Int = elements.length

  def isEmpty: Boolean = size == 0

  def clear() {
    var n = capacity - 1
    while (n >= 0) {
      elements(n) = null
      n -= 1
    }
  }

  def contains(element: E): Boolean = {
    val wrapped = Option(element)
    var n = capacity - 1
    while (n >= 0) {
      if (elements(n) == wrapped) {
        return true

      } else {
        n -= 1
      }
    }

    false
  }

  private def grow() {
    val newCapacity = (elements.length * 3) / 2
    grow(newCapacity)
  }

  private def grow(newCapacity: Int) {
    val oldData = elements
    elements = Array.fill[Option[E]](newCapacity)(null)
    Array.copy(oldData, 0, elements, 0, oldData.length)
  }
}
