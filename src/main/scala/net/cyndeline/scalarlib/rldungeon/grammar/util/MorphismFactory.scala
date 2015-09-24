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
   * @tparam VType Type of vertex mapped in the morphism.
   * @return A morphism wrapping the supplied map.
   */
  def build[VType](mapping: Map[VType, VType]): Morphism[VType] = new Morphism(mapping)

}
