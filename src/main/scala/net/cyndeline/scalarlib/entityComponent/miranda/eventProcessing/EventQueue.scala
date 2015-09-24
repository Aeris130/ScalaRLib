package net.cyndeline.scalarlib.entityComponent.miranda.eventProcessing

import scala.collection.mutable
import net.cyndeline.scalarlib.entityComponent.miranda.{Entity, Event}
import net.cyndeline.scalarlib.entityComponent.miranda.util.Bag

/**
 * Stores events in a queue and process them by sending them in the order they were received.
 *
 * Events may also be sent as replies to other events. This queue gives events that were sent as replies higher priority
 * than other events, causing replies to be processed immediately after the event they replies to.
 *
 * Example: Events A, B, C are sent to the queue, processing A yields replies D, E. Processing E yields F.
 * Processing B yields G, H.
 *
 * The order that the events will be returned in is: A, D, E, F, B, G, H, C
 */
class EventQueue {
  private val orderQueues = new Bag[mutable.Queue[(Entity, Event)]]()
  private var highestOrder = 0

  // Insert a queue for the base order
  orderQueues.set(0, mutable.Queue[(Entity, Event)]())

  /** Inserts an event in the queue. */
  def enqueue(event: Event, entity: Entity) {
     val queue = orderQueues.get(event.replyOrder).getOrElse {
       val queue = mutable.Queue[(Entity, Event)]()
       orderQueues.set(event.replyOrder, queue)
       queue
     }

    if (event.replyOrder > highestOrder)
      highestOrder = event.replyOrder

    queue.enqueue((entity, event))
  }

  /** Removes the next event from the queue and returns it. Throws error if queue is empty. */
  def dequeue(): (Entity, Event) = {
    val queue = orderQueues.get(highestOrder).getOrElse {
      throw new Error("The highest order bag entry did not contain a queue.")
    }
    val resultEvent = queue.dequeue()

    if (queue.isEmpty && highestOrder > 0)
      highestOrder -= 1

    resultEvent
  }

  /** Checks if any more events are in the queue. */
  def hasNext: Boolean = {
    !orderQueues.get(highestOrder).getOrElse {
      throw new Error("The highest order bag entry did not contain a queue at order " + highestOrder + ".")
    }.isEmpty
  }

}
