package net.cyndeline.scalarlib.entityComponent.miranda

import scala.collection.mutable.ArrayBuffer
import net.cyndeline.scalarlib.entityComponent.miranda.util.Bag

/**
 * Sends events and the entities they are being sent to into the system handlers.
 */
trait EventHandler {

  /** Sends 1 event along with its entity to every system that accepts both the event and the entity.
    * @return True if an event was processed, false if no events were available.
    */
  def process: Boolean

}
