package rlgraph.help

import net.cyndeline.scalarlib.rlgraph.face.Face
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import net.cyndeline.scalarlib.rlgraph.embedding.Embedding
import net.cyndeline.scalarlib.rlgraph.embedding.immutable.UndirectedEmbedding

/**
 * Embeds a circular face.
 */
object FaceEmbedder {
  def embeddingFrom[VType : TypeTag : ClassTag](f: Face[VType]): Embedding[VType] = {
    var embedding: Embedding[VType] = UndirectedEmbedding[VType]()
    for (edge <- f.edges)
      embedding = embedding.embed(edge._1, edge._2)

    embedding
  }
}
