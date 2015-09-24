package net.cyndeline.scalarlib.rldungeon.common

/**
 * Assigns a unique id to edges in a graph.
 */
trait Corridor {

  /**
   * @return A unique ID associated with this corridor (in the sense that no other corridor in any given graph structure
   *         shares the same ID).
   */
  def cid: Int

}
