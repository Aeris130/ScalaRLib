package net.cyndeline.scalarlib.rlgraph.regularEdgeLabeling.factories

import net.cyndeline.scalarlib.rlgraph.regularEdgeLabeling.{EdgeLabeling, RegularEdgeLabeling, RELFactory}
import net.cyndeline.scalarlib.rlgraph.embedding.Embedding
import net.cyndeline.scalarlib.rlgraph.regularEdgeLabeling.factories.help.CanonicalEdgeSets
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag

/**
 * Produces a regular edge labeling by sorting vertices according to their canonical ordering. The algorithm is
 * implemented as described by G.Kant in the article "Algorithms for drawing planar graphs".
 */
class CanonicalOrderFactory extends RELFactory {

  def produceRegularEdgeLabeling[V : TypeTag : ClassTag](embedding: Embedding[V], north: V, south: V, west: V, east: V): EdgeLabeling[V] = {
    val edgeSets = new CanonicalEdgeSets(embedding, south, north, west, east)
    RegularEdgeLabeling(edgeSets.T1, edgeSets.T2, embedding, edgeSets.embeddingT1, edgeSets.embeddingT2, north, west, south, east)
  }

}
