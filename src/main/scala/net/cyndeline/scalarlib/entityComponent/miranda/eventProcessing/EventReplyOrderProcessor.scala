package net.cyndeline.scalarlib.entityComponent.miranda.eventProcessing

import net.cyndeline.scalarlib.entityComponent.miranda._
import scala.collection.mutable.ArrayBuffer
import net.cyndeline.scalarlib.entityComponent.miranda.util.Bag

/**
 * Respects the order that response-events are sent in when deciding the processing order of all events.
 */
class EventReplyOrderProcessor() extends EventHandlerSetup {
  private val eventQueue = new EventQueue()
  private var systemBuffers: Bag[ArrayBuffer[EventSystem]] = null
  private var entityManager: EntityComponentManager = null

  /** Sends 1 event along with its entity to every system that accepts both the event and the entity.
    * @return True if an event was processed, false if no events were available.
    */
  def process: Boolean = {
    if (!eventQueue.hasNext) return false

    val eventAndEntity = eventQueue.dequeue()
    val entity = eventAndEntity._1
    val event = eventAndEntity._2
    val systems = systemBuffers.get(event.id).getOrElse {
      throw new Error("The event " + event + " was sent, but no system subscribes to it.")
    }

    if (!systems.isEmpty) {
      val size = systems.size
      var i = 0
      while (i < size && !event.isCancelled) {
        val sys = systems(i)
        if (sys.isEnabled && hasValidComponentSet(entity, sys))
          sys.onEvent(entity, event)

        i += 1
      }

      true
    } else {
      throw new Error("The event " + event + " was sent, but no system subscribes to it.")
    }

  }

  /** Used by the entity manager to send events here. */
  def send(event: Event, entity: Entity) {
    eventQueue.enqueue(event, entity)
  }

  def setSystemBuffer(systems: Bag[ArrayBuffer[EventSystem]]) {
    systemBuffers = systems
  }

  def setEntityManager(em: EntityComponentManager) {
    entityManager = em
  }

  private def hasValidComponentSet(entity: Entity, system: EventSystem): Boolean = {
    val allIncludedComponents = system.subscription.includeAllTypes.iterator
    while (allIncludedComponents.hasNext) {
      val component: ComponentType[_ <: Component] = allIncludedComponents.next()
      if (!entityManager.hasComponent(entity, component)) return false
    }

    true
  }
}
