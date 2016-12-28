package net.cyndeline.scalarlib.rldrawing.continent

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}
import net.cyndeline.rlcommon.util.Matrix2DOp
import net.cyndeline.scalarlib.rldrawing.MapLayout
import net.cyndeline.scalarlib.rldrawing.common.RRoom
import net.cyndeline.scalarlib.rldrawing.forceGrid.ForceGridLayout
import net.cyndeline.scalarlib.rldrawing.util.noise.{DistanceMask, FBMotion, Simplex2D}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.GraphTraversal.{Parameters, Successors}
import scalax.collection.immutable.Graph

/**
  * Generates continents that may or may not be surrounded by water on all sides. The basis of the algorithm is
  * simplex noise, generated with fractal brownian motion to create the organic randomness that is sought after when
  * producing noise-based maps. A radial gradient drop-off is then used to filter out land towards the edges of the map,
  * creating a continent in the center.
  */
class ContinentFactory(settings: ContinentSettings) {

  /**
    * @param width Width of the map to generate.
    * @param height Height of the map to generate.
    * @param areaSize Defaults to 3. Can be set higher to scale each area on the map, that will otherwise be represented
    *                 as a 3x3 rectangular room.
    * @return The generated continent map.
    */
  def build(width: Int, height: Int, seed: Int, areaSize: Int = 3): MapLayout[RRoom] = {
    require(width > 0 && height > 0, "Map width and height must be > 0.")
    require(width < Int.MaxValue - 1 && height < Int.MaxValue - 1,
      "Width and height must be <= Int.Max - 2, in order to save a coordinates water margin on all four sides of the island.")
    require(areaSize >= 3, "Area size must be >= 3.")

    // Add 2 coordinates to width and height to leave a strand of water outside the map
    val finalWidth = width + 2
    val finalHeight = height + 2
    val noiseGrid = generateNoise(finalWidth, finalHeight, seed)
    val grid = Array.ofDim[Int](finalWidth, finalHeight) // 1 = land, 0 = water, -1 = final water (see below)
    val visited = Array.ofDim[Boolean](finalWidth, finalHeight)

    /* Start by generating the noise used to determine of a coordinate should contain land or water. If the noise
     * on a given coordinate is greater than the water height, it becomes land.
     *
     * The lowest noise value must be computed before the mask is added, otherwise the factory will "fill out" the
     * continent towards the border as far as it needs to in order to have the correct ratio.
     */
    val requiredHeight = computeWaterHeight(noiseGrid)
    applyMask(noiseGrid)
    def setLandType(v: Int, x: Int, y: Int): Int = {
      visited(x)(y) = false
      if (x == 0 || y == 0 || x == finalWidth - 1 || y == finalHeight - 1) { // Border case
        -1 // Will be water in the final step

      } else {
        val noise = noiseGrid(x)(y)
        if (noise > requiredHeight || settings.waterMass == 0) 1
        else 0
      }
    }
    Matrix2DOp.modify(grid, setLandType _)

    /* The noise will generate water inside the island as well. To find the contour, do a floodfill from a corner, since
     * they're guaranteed to have water on them. Any water found this way gets marked -1. After that, any remaining
     * bodies of water are marked as land.
     *
     * Scala's graph library is utilized for this. A BFS traversal is done starting at (0,0), which is guaranteed to
     * be connected to the outer water since a margin of 1 coordinate was added around the map. Every node not visited
     * from (0,0) is either land, or water surrounded by land (i.e not the outer ocean) and is marked as land.
     */
    def landFilter(d: (Int, Int)): Boolean = grid(d._1)(d._2) != 1 // Keeps land from being added

    val edges = new ArrayBuffer[UnDiEdge[(Int, Int)]]()
    for (i <- 0 until finalWidth; j <- 0 until finalHeight if grid(i)(j) != 1) {
      val a = (i, j)
      val neighbors: Vector[(Int, Int)] = allDirections(finalWidth, finalHeight)(i, j, landFilter)
      for (n <- neighbors)
        edges += a~n
    }
    val mapGraph = Graph.from(Nil, edges)
    val root = mapGraph get (0, 0)
    val bfsTraverser = root.outerNodeTraverser(Parameters.Bfs(Successors, Integer.MAX_VALUE))
    for (waterCoordinate <- bfsTraverser) {
      visited(waterCoordinate._1)(waterCoordinate._2) = true
    }

    /* Remove any landmasses whose mass is less than X% of the largest mass. This removes stray island artifacts. */
    val landmasses = computeLandmasses(visited)
    if (landmasses.isEmpty) {
      return ForceGridLayout.empty
    }

    val largest = landmasses.maxBy(_.size)
    val largestSize = largest.size
    val smallestAllowed = largestSize * settings.minIslandMass
    val tooSmall = if (settings.removeIslands) landmasses.filter(_ != largest)
                   else landmasses.filter(m => m.size < smallestAllowed)

    for (smallIsland <- tooSmall; coordinate <- smallIsland) {
      visited(coordinate._1)(coordinate._2) = true // Set the land of the island to water
    }

    /* Since not every cell in the grid may end up added as an area, it wont be possible to convert back and forth
     * between the indices in the area vector, and the coordinates in the grid. Hence we create a grid where every
     * cell that is being added as an area stores its position in the final vector.
     *
     * We also store a list of the original area coordinates in the same order as they're added as areas to the final
     * vector.
     */
    val indexGrid = Array.fill(finalWidth, finalHeight)(-1)
    val originalCoords = new ArrayBuffer[(Int, Int)]()

    // Create areas from every non-visited coordinate
    var currentIndex = 0
    val areas = (for (i <- 0 until finalWidth; j <- 0 until finalHeight if i > 0 && j > 0 && i < finalWidth - 1 && j < finalHeight - 1 && !visited(i)(j)) yield {
      val x = indexToCoordinate(i, areaSize)
      val y = indexToCoordinate(j, areaSize)
      val start = Point(x, y)
      indexGrid(i)(j) = currentIndex
      originalCoords += ((i, j))
      currentIndex += 1
      Rectangle(start, areaSize, areaSize)
    }).toVector

    ForceGridLayout.withConnectionGraph(areas, Vector(), makeConnectionGraph(originalCoords, visited, indexGrid))
  }

  private def generateNoise(width: Int, height: Int, seed: Int): Array[Array[Double]] = {
    val simplex = new FBMotion(Simplex2D(seed)) // Generates noise values between 0 and 1
    val grid = Array.ofDim[Double](width, height)
    var i = 0
    while (i < width) {
      var j = 0
      while (j < height) {
        grid(i)(j) = simplex.sumOctave(settings.frequency * i, settings.frequency * j, settings.iterations, settings.persistence, settings.scale)
        j += 1
      }
      i += 1
    }

    grid
  }

  def applyMask(noise: Array[Array[Double]]): Unit = if (settings.maskBorder) {
    val maskedNoise = DistanceMask(settings.elevationIncrease, settings.elevationDecrease, settings.elevationDropOff)
      .withMultiplication.withEuclideanDistance.applyMask(noise)
    Matrix2DOp.modify(noise, (_: Double, x: Int, y: Int) => maskedNoise(x)(y))
  }

  /**
    * Sorts all noise values (lowest first) then selects a value using an index that depends on the percentage of
    * water (if the water mass is 40%, the fourth index in an array with 10 values will be selected, causing 4/10
    * values to end up below the land threshold).
    * @param grid The final noise grid.
    * @return The highest value that should be parsed into water.
    */
  private def computeWaterHeight(grid: Array[Array[Double]]): Double = {
    val allValues = grid.flatten.sorted
    allValues(((allValues.length - 1) * settings.waterMass).toInt)
  }

  /**
    * Uses the fact that every rectangle has the same dimensions to check n/w/s/e coordinates for existing neighbors.
    *
    * Arrays are used instead of maps to speed up the runtime.
    *
    * @param areas A vector containing the x/y coordinates of the initial grid that the noise was applied to, rather
    *              than the rectangle start coordinates. Each coordinate pair is stored on the same index as
    *              the rectangular area will be at in the final layout.
    * @param index A version of the initial grid where each cell instead stores the index of that area in the area
    *              vector, or -1 if the cell isn't among the areas.
    */
  private def makeConnectionGraph(areas: ArrayBuffer[(Int, Int)],
                                  visited: Array[Array[Boolean]],
                                  index: Array[Array[Int]]): Graph[Int, UnDiEdge] = {
    val n = areas.length
    val edges = new ArrayBuffer[UnDiEdge[Int]](n * 8)
    val singleNodes = new ArrayBuffer[Int](n)
    val width = visited.length
    val height = visited(0).length

    def addNeighbor(from: Int, to: Int): Unit = {
      edges += from ~ to
    }
    var i = 0
    while (i < n) {
      /* Each area only needs to check the neighbors to the north and east. They themselves will be discovered by their
       * south/west neighbors.
       */
      val coordinate = areas(i)
      val northCoordinate = (coordinate._1, coordinate._2 + 1)
      val eastCoordinate = (coordinate._1 + 1, coordinate._2)
      var nExists = false

      if (northCoordinate._1 < width && northCoordinate._2 < height && index(northCoordinate._1)(northCoordinate._2) != -1) {
        addNeighbor(index(coordinate._1)(coordinate._2), index(northCoordinate._1)(northCoordinate._2))
        nExists = true
      }

      if (eastCoordinate._1 < width && eastCoordinate._2 < height && index(eastCoordinate._1)(eastCoordinate._2) != -1) {
        addNeighbor(index(coordinate._1)(coordinate._2), index(eastCoordinate._1)(eastCoordinate._2))
        nExists = true
      }

      if (!nExists)
        singleNodes += index(coordinate._1)(coordinate._2)

      i += 1
    }

    Graph.from(singleNodes, edges)
  }

  /** Computes every connected landmass as a set of its coordinates (diagonal movement not allowed).
    * @param water A matrix where (x,y) is true if that coordinate contains water.
    */
  private def computeLandmasses(water: Array[Array[Boolean]]): Vector[Set[(Int, Int)]] = {
    if (water.length == 0)
      return Vector()

    val width = water.length
    val height = water(0).length
    val processed = Array.fill(width, height)(false)

    def markWaterAsProcessed(dummy: Boolean, x: Int, y: Int): Boolean = {
      water(x)(y)
    }
    Matrix2DOp.modify(processed, markWaterAsProcessed _)

    val landmasses = new ArrayBuffer[Set[(Int, Int)]]()

    def directions = allDirections(width, height) _
    def notProcessed(c: (Int, Int)) = !processed(c._1)(c._2)
    def floodFillTraversal(current: (Int, Int), remaining: mutable.Queue[(Int, Int)], landMass: mutable.HashSet[(Int, Int)]): Unit = {
      landMass += current
      val dir = directions(current._1, current._2, notProcessed)

      for (d <- dir)
        processed(d._1)(d._2) = true

      if (dir.nonEmpty)
        remaining.enqueue(dir:_*)

      if (remaining.nonEmpty)
        floodFillTraversal(remaining.dequeue(), remaining, landMass)
    }

    def lookForStart(p: Boolean, x: Int, y: Int): Boolean = {
      if (!water(x)(y) && !processed(x)(y)) {
        val q = new mutable.Queue[(Int, Int)]()
        val m = new mutable.HashSet[(Int, Int)]()
        processed(x)(y) = true
        floodFillTraversal((x, y), q, m)
        landmasses += m.toSet
      }
      true
    }

    Matrix2DOp.modify(processed, lookForStart _)
    landmasses.toVector
  }

  /* Subtract index to make rectangles share a border */
  private def indexToCoordinate(index: Int, areaSize: Int): Int = if (areaSize > 1) index * areaSize - index else index

  private def allDirections(width: Int, height: Int)(x: Int, y: Int, filter: ((Int, Int)) => Boolean): Vector[(Int, Int)] = {
    Vector(
      (x - 1, y), // W
      (x, y + 1), // N
      (x + 1, y), // E
      (x, y - 1) // S
    ).filter(d => d._1 >= 0 && d._1 < width && d._2 >= 0 && d._2 < height && filter(d))
  }

}

/**
  * Settings for generating continents. The three elevation parameters controls the island border mask (a, b, c),
  * see DistanceMask class for description.
  *
  * Note that when using island masks, the mask will remove land areas, causing the water-to-land ratio to exceed
  * the specified water mass parameter. Otherwise there may arise situations where the map receives a square shape
  * due to not enough water-spaces being available.
  *
  * @param waterMass A value in the range [0, 1] that determines approximately how much of the final level that
  *                  should be water. 0 = no water, 1 = all water.
  * @param scale Scales the noise values to reduce frequency and create smoother changes in level. A low value in the
  *              range 0.1 to 0.0001 is recommended.
  * @param persistence Controls how fast the significance of each iteration drops off during the fractal brownian
  *                    motion. Suggested value 0.5.
  * @param iterations The number of noise-values that should be generated for each coordinate. Used in fractal brownian
  *                   motion. Suggested value 16.
  * @param maskBorder If set to true, the settings will be used to produce an island that doesn't stretch its landmass
  *                   all the way to the square border.
  * @param removeIslands If set to true, only the largest landmass will be kept.
  * @param minIslandMass If islands are not removed by the algorithm, this value between 0 and 1 determines the smallest
  *                      possible island mass compared to the largest landmass. Example: If the value is 0.3 and the
  *                      largest landmass has 1000 areas in it, islands smaller than 300 (30% of 1000) will be removed.
  * @param frequency Controls how fine the noise pattern is, relative to the scale of the map. A larger frequency
  *                  results in smaller extremites, but also a more blob-like map if the island mask is active.
  * @param elevationIncrease a, see DistanceMask class.
  * @param elevationDecrease b, see DistanceMask class.
  * @param elevationDropOff c, see DistanceMask class.
  */
class ContinentSettings(val waterMass: Double,
                        val scale: Double,
                        val persistence: Double,
                        val iterations: Int,
                        val maskBorder: Boolean,
                        val removeIslands: Boolean,
                        val minIslandMass: Double,
                        val frequency: Double,
                        val elevationIncrease: Double,
                        val elevationDecrease: Double,
                        val elevationDropOff: Double) {

  require(waterMass >= 0 && waterMass <= 1, "Water mass must be in the range [0, 1].")
  require(minIslandMass >= 0 && minIslandMass <= 1, "Island mass must be in the range [0, 1].")
}

object ContinentSettings {
  private val defaultFrequency = 1
  private val defaultElevationIncrease = 0.05
  private val defaultElevationDecrease = 1
  private val defaultElevationDropOff = 1.5

  def apply(waterMass: Double, scale: Double, persistence: Double, iterations: Int, maskBorders: Boolean) =
    new ContinentSettings(waterMass, scale, persistence, iterations, false, false, 1, defaultFrequency, defaultElevationIncrease, defaultElevationDecrease, defaultElevationDropOff)
}
