package net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.orthogonal.network.util

import scalax.collection.immutable.Graph
import scala.collection.mutable
import net.cyndeline.scalarlib.rlgraph.face.Face

/**
 * Stores the graph representation of a flow network, as well as additional
 * data used when generating it.
 *
 * @constructor Creates a new mutable network representation container.
 */
class NetworkRepresentation[VType] {

  var graph: Graph[FlowVertex, FlowEdge] = null
  var externalFace: FlowVertex = null

  val vertexMapping = new mutable.HashMap[FlowVertex, VType]()
  val faceMapping = new mutable.HashMap[FlowVertex, Face[VType]]()

  override def toString: String = {
    val builder = new StringBuilder()

    if (graph != null)
      builder ++= "Graph: " + graph.nodes.mkString(", ") + " ; " + graph.edges.mkString(", ")
    else
      builder ++= "No graph"

    builder ++= " | "

    if (externalFace != null)
      builder ++= "Ext face: " + externalFace
    else
    builder ++= "No external face"

    builder ++= " | "
    builder ++= vertexMapping.mkString(", ") + " | " + faceMapping.mkString(", ")
    builder.toString()
  }
}
