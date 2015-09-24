package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * A class to be extended by users in order to modify entities after they've been assembled by a blueprint.
 *
 * @param arguments A sequence of string keys. Entities matching said keys will be sent to be processed in the same
 *                  order as the keys are ordered here.
 */
abstract class BlueprintProcessor(val arguments: String*) {
  duplicateArguments()

  /**
   * @param entities A sequence of entities corresponding to the supplied key arguments.
   */
  def processEntities(entities: Seq[Entity]): Unit

  private def duplicateArguments() {
    val duplicates = arguments.groupBy(identity).collect { case (x, List(_,_,_*)) => x }
    require(duplicates.isEmpty, "Duplicate entity key arguments found: " + duplicates.mkString(", "))
  }
}
