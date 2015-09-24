package net.cyndeline.scalarlib.entityComponent.miranda.entityManager.eventCreation

import net.cyndeline.scalarlib.entityComponent.miranda.Event
import java.lang.reflect.Constructor

/**
 * Instantiates new events from scratch every time an event-instantiation is requested.
 *
 * @param constructor Empty constructor that instantiates new events with default values.
 * @param id Unique id for the event class.
 */
class NewEventFactory[T <: Event](constructor: Constructor[_], id: Int) extends EventFactory[T] {
  def createEvent: T = {
    val event = constructor.newInstance().asInstanceOf[T]
    event.id = id
    event
  }
}
