package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

/**
 * Represents a vertex in a graph, or multiple merged vertices.
 *
 * @constructor Constructs a node from a set of vertices.
 * @param vertexCollection All vertices represented by this node. Must be non-empty.
 * @param isDummy True if the node represents a single cutpoint, joining other biconnected components with more
 *                than 2 edges.
 */
case class CollapsedNode(vertexCollection: Set[Int], isDummy: Boolean = false) {
  require(vertexCollection.nonEmpty, "Attempted to initiate collapsed node with empty vertex set.")

  /**
   * Constructs a new collapsed node with a sequence of vertices it represents.
   * @param v A vertex represented by this node.
   * @param vs Additional vertices if this is a super-node.
   */
  def this(v: Int, vs: Int*) = this(vs.toSet + v, false)

  /**
   * Constructs a node from a single vertex.
   * @param v Vertex to add to the node.
   */
  def this(v: Int) = this(Set(v), false)

  def asDummy: CollapsedNode = CollapsedNode(vertexCollection, true)

  /**
   * Retrieves a single vertex if this node represents one. Use this whenever a single vertex is to be retrieved,
   * as the method fails fast if more/less than one vertex is present.
   * @return The single vertex represented by this node.
   */
  def singleRepresentedVertex: Int = {
    if (vertexCollection.size == 1)
      vertexCollection.head
    else
      throw new Error("the collapsed vertex " + this + " does not represent a single vertex.")
  }

  /**
   * @return True if this node represents more than one vertex, otherwise false.
   */
  def isSuperNode: Boolean = vertexCollection.size > 1

  /**
   * Checks if this node represents a specific vertex.
   * @param vertex Vertex id to check representation status for.
   * @return True if the vertex is represented by this node, otherwise false.
   */
  def contains(vertex: Int): Boolean = vertexCollection.contains(vertex)

  /**
   * Checks if this node represents a set of vertices of which some make up the
   * specified subset.
   * @param set A set of vertices.
   * @return True if this node represents every vertex (or more) in the specified set.
   */
  def containsSubset(set: Set[Int]): Boolean = set subsetOf vertexCollection

  override def toString: String = "[CN: " +  vertexCollection.mkString(",") + (if (isDummy) "|D" else "") + "]"

}
