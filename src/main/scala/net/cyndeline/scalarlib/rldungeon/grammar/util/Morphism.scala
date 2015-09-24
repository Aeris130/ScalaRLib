package net.cyndeline.scalarlib.rldungeon.grammar.util

/**
 * Maps the relation between the vertices in a graph pattern, and the vertices in a graph the pattern matches.
 *
 * @tparam VType Type of vertex represented in the morphism.
 */
class Morphism[VType](mapping: Map[VType, VType]) {

  def this() = this(Map[VType, VType]())

  /**
   * @return The number of vertices in the morphism.
   */
  def size = mapping.size

  /**
   * Retrieves a vertex in the morphism, or fails fast with a message.
   * @param v Vertex whose corresponding vertex should be retrieved.
   * @return The vertex mapped against the supplied vertex in the morphism.
   */
  def getVertexCorrespondingTo(v: VType): VType = mapping.getOrElse(v, throw new NoSuchElementException("No morphism defined for vertex " + v))

}
