package net.cyndeline.scalarlib.entityComponent.miranda.entityManager.eventCreation

import java.lang.reflect.Constructor

import net.cyndeline.scalarlib.entityComponent.miranda.Event

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
