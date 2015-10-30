package net.cyndeline.scalarlib.rldungeon.grammar.util

/**
 * Produces morphisms.
 *
 * @constructor Creates a new morphism factory.
 */
class MorphismFactory {

  /**
   * Builds a new morphism.
   * @param mapping Map with the left and right-hand side of the morphism.
   * @tparam LevelVertex Type of rooms used in level to apply production on.
   * @tparam PatternVertex Vertex type used in pattern graphs.
   * @return A morphism wrapping the supplied map.
   */
  def build[LevelVertex, PatternVertex](mapping: Map[PatternVertex, LevelVertex]): Morphism[LevelVertex, PatternVertex] = new Morphism(mapping)

}
