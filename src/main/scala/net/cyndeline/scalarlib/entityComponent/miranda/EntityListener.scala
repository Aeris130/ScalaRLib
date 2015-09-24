package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * User-supplied class that subscribes to modifications to the entity data set. Allows the user to synchronize
 * collections that uses entity ids, making sure that no entity creation/deletions are missed.
 *
 * Note that expensive computations can severely hamper overall system performance.
 */
trait EntityListener {

  /**
   * Called whenever an entity is created. If the entity was created using an assemblage, all components will be added
   * before this call.
   * @param entity The entity that was created. Can safely be used in system queries.
   */
  def onCreate(entity: Entity)

  /**
   * Called whenever an entity is deleted.
   * @param entity The entity that is about to be deleted. This reference is still valid as input in
   *               system queries.
   */
  def onDelete(entity: Entity)

}
