package net.cyndeline.scalarlib.rldungeon.grammar.util

/**
 * Maps the relation between the vertices in a graph pattern, and the vertices in a graph the pattern matches.
 *
 * @tparam LevelVertex Type of rooms used in level to apply production on.
 * @tparam PatternVertex Vertex type used in pattern graphs.
 */
class Morphism[LevelVertex, PatternVertex](mapping: Map[PatternVertex, LevelVertex]) {

  def this() = this(Map())

  /**
   * @return The number of vertices in the morphism.
   */
  def size = mapping.size

  /**
   * Retrieves a vertex in the morphism, or fails fast with a message.
   * @param vertexInPattern Known vertex in a pattern, whose corresponding vertex in the level-graph should be
   *                        retrieved.
   * @return The vertex mapped against the supplied vertex in the morphism.
   */
  def matching(vertexInPattern: PatternVertex): LevelVertex =
    mapping.getOrElse(vertexInPattern, throw new NoSuchElementException("No morphism defined for vertex " + vertexInPattern))

}
