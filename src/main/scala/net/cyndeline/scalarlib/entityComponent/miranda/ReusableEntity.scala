package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Represents an entity identifier. By making this instance unique, it allows the entity manager to kill entities
 * as they are removed (and their id's recycled) from the system.
 */
class ReusableEntity(val id: Int, manager: EntityComponentManager) extends Entity {
  def this(id: Int) = this(id, null)

  var alive = true

  /** Makes this container illegal to use when polling for component data. */
  def kill() { alive = false }

  override def equals(other: Any): Boolean = other match {
    case e: Entity => id == e.id
    case _ => false
  }

  override def hashCode: Int = id
}
