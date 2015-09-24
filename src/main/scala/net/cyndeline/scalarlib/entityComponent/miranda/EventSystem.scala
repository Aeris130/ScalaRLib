package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Handles events when sent to an entity.
 */
trait EventSystem {
  private var enabled = true

  /**
   * The method that gets invoked every time an event that this system subscribes to is sent to an entity
   * with a component set that matches the requirements of the system.
   * @param entity Id of entity that the event is sent to.
   * @param event The event being sent.
   */
  def onEvent(entity: Entity, event: Event)

  /** Determines which events and components the system is interested in. */
  def subscription: Subscription

  /** Marks this system as eligible for events. */
  def enable() { enabled = true }

  /** Disables events from being passed to this system. */
  def disable() { enabled = false }

  /** True if the system accepts events, otherwise false. */
  def isEnabled: Boolean = enabled

}
