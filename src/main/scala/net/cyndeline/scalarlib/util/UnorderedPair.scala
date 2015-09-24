package net.cyndeline.scalarlib.util

/**
 * A tuple of elements whose equality isn't dependent on order.
 */
class UnorderedPair[E](a: E, b: E) {
  private val thisHash = if (a.## < b.##) (a, b).## else (b, a).##

  def _1: E = a
  def _2: E = b

  def asTuple: (E, E) = (a, b)

  def contains(e: E): Boolean = _1 == e || _2 == e

  override def equals(other: Any): Boolean = other match {
    case p: UnorderedPair[E] => (p._1 == a && p._2 == b) || (p._1 == b && p._2 == a)
    case _ => false
  }

  override def hashCode: Int = thisHash

  override def toString: String = "Unordered(" + a + ", " + b + ")"

}

object UnorderedPair {
  def apply[E](a: E, b: E) = new UnorderedPair(a, b)
  def apply[E](pair: (E, E)) = new UnorderedPair(pair._1, pair._2)
}
