package net.cyndeline.scalarlib.entityComponent.miranda

import scala.collection.mutable.ArrayBuffer
import net.cyndeline.scalarlib.entityComponent.miranda.util.Bag

/**
 * Specifies setup methods for event handlers.
 */
trait EventHandlerSetup extends EventHandler {

  /** Sends a message to an entity to be processed. */
  def send(event: Event, entity: Entity)

  /** Sets the buffer of systems to use for each event index. */
  def setSystemBuffer(systems: Bag[ArrayBuffer[EventSystem]])

  /** Sets the manager to be used when checking component data. */
  def setEntityManager(em: EntityComponentManager)

}
