package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Representation for an entity instance, used as key to retrieve component data from the manager.
 */
trait Entity {

  /**
   * Unique identifier for this entity. Do not use this to reference it, as it will make it impossible to
   * tell if the entity has had its id reused.
   */
  def id: Int
}
