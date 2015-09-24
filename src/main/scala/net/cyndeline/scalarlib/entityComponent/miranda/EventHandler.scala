package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Sends events and the entities they are being sent to into the system handlers.
 */
trait EventHandler {

  /** Sends 1 event along with its entity to every system that accepts both the event and the entity.
    * @return True if an event was processed, false if no events were available.
    */
  def process: Boolean

}
