package net.cyndeline.scalarlib.rlgraph.cycles.cycleBase.jGraphT

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import org.jgrapht.alg.cycle.PatonCycleBase
import org.jgrapht.UndirectedGraph
import org.jgrapht.graph.DefaultEdge
import java.util
import scala.collection.JavaConversions._
import net.cyndeline.scalarlib.rlgraph.cycles.cycleBase.UndirectedCycleBaseFinder
import net.cyndeline.scalarlib.util.graphConverters.jGraphT.ScalaGraphToJGraphTConverter

/**
 * Wrapper for the PatonCycleBase algorithm in the JGraphT library.
 *
 * Has a worst case time complexity is O(n^3), where n is the number of vertices in the graph.
 *
 * @constructor Creates a new undirected cycle-base finder.
 */
class PatonCycle extends UndirectedCycleBaseFinder {

  def findCycleBase[VType, EType[X] <: UnDiEdge[X]](graph: Graph[VType, EType]): Vector[Vector[VType]] = {
    val converter = new ScalaGraphToJGraphTConverter()
    val undirectedGraph: UndirectedGraph[VType, DefaultEdge] = converter.convert(graph)
    val cycleFinder = new PatonCycleBase[VType, DefaultEdge](undirectedGraph)
    val cycles: util.List[util.List[VType]] = cycleFinder.findCycleBase()

    var convertedCycleList = Vector[Vector[VType]]()
    val allCycles = cycles.iterator()
    while (allCycles.hasNext) {
      val c = allCycles.next()
      val convertedList: Vector[VType] = c.toVector // Java -> Scala
      convertedCycleList = convertedCycleList :+ convertedList
    }

    convertedCycleList
  }
}
