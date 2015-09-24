package entityComponent.miranda

import net.cyndeline.scalarlib.entityComponent.miranda._
import net.cyndeline.scalarlib.entityComponent.miranda.eventProcessing.EventReplyOrderProcessor
import net.cyndeline.scalarlib.entityComponent.miranda.util.Bag
import testHelpers.SpecImports

import scala.collection.mutable.ArrayBuffer

class EventReplyOrderProcessorSpec extends SpecImports {

  class TestEvent(order: Int) extends Event {
    def this() = this(0)
    replyOrder = order
  }

  class TestComponent() extends Component

  def eventProcessor(enabled: Boolean) = new {
    val processor = new EventReplyOrderProcessor()

    // System setup
    val system = mock[EventSystem]
    val subscription = new Subscription(Set(classOf[TestEvent]))
    subscription.includeComponent(classOf[TestComponent])
    val componentType = new ComponentType[TestComponent](0, "TestType")
    subscription.includeAllTypes = Set(componentType)
    (system.isEnabled _) expects() returns(enabled) anyNumberOfTimes()
    (system.subscription _) expects() returns(subscription) anyNumberOfTimes()

    // Entity component manager
    val entityManager = mock[EntityComponentManager]
    processor.setEntityManager(entityManager)

    val systemBuffer = new ArrayBuffer[EventSystem]() += system
    val systems = new Bag[ArrayBuffer[EventSystem]]()
    systems.set(0, systemBuffer) // Events with id 0
    processor.setSystemBuffer(systems)
  }

  describe("EventReplyOrderProcessor") {

    it ("should process events of the same reply order as they are sent") {

      Given("an event processor")
      val f = eventProcessor(true)
      import f._
      (entityManager.hasComponent[TestComponent] _) expects(*, *) returns(true) anyNumberOfTimes()

      When("sending three events of the same reply order")
      val eventId = 0
      val replyOrder = 0
      val event1 = new TestEvent(replyOrder)
      val event2 = new TestEvent(replyOrder)
      val event3 = new TestEvent(replyOrder)
      event1.id = eventId
      event2.id = eventId
      event3.id = eventId
      val entity = new ReusableEntity(1)
      processor.send(event1, entity)
      processor.send(event2, entity)
      processor.send(event3, entity)

      Then("the events should be processed in the order they were sent")
      inSequence {
        (system.onEvent _) expects(entity, event1) returns() once()
        (system.onEvent _) expects(entity, event2) returns() once()
        (system.onEvent _) expects(entity, event3) returns() once()
      }

      // Start test
      processor.process
      processor.process
      processor.process

    }

    it ("should not process systems that are disabled") {

      Given("a processor with a system that is disabled")
      val f = eventProcessor(false)
      import f._

      When("sending an event to the processor")
      val event1 = new TestEvent()
      event1.id = 0
      val entity = new ReusableEntity(1)
      processor.send(event1, entity)

      Then("Nothing should happen")
      // Mock isn't throwing exceptions here

      // Start test
      processor.process

    }

    it ("should process when no events are present") {

      Given("a processor")
      val f = eventProcessor(true)
      import f._

      When("processing without any events being in the queue")
      val result = processor.process

      Then("the result should be false")
      result should be (false)

    }

    it ("should process events according to their response order") {

      Given("5 events A,B,C,D,E where C and E are responses to an order-0 event, and D is response to an order-2 event")
      val f = eventProcessor(true)
      import f._
      (entityManager.hasComponent[TestComponent] _) expects(*, *) returns(true) anyNumberOfTimes()

      val eventId = 0
      val A = new TestEvent(0)
      val B = new TestEvent(0)
      val C = new TestEvent(1)
      val D = new TestEvent(2)
      val E = new TestEvent(1)
      A.id = eventId
      B.id = eventId
      C.id = eventId
      D.id = eventId
      E.id = eventId
      val entity = new ReusableEntity(1)
      processor.send(A, entity)
      processor.send(B, entity)
      processor.send(C, entity)
      processor.send(D, entity)
      processor.send(E, entity)

      When("processing the events")
      Then("the order should be D, C, E, A, B")
      inSequence {
        (system.onEvent _) expects(entity, D) returns() once()
        (system.onEvent _) expects(entity, C) returns() once()
        (system.onEvent _) expects(entity, E) returns() once()
        (system.onEvent _) expects(entity, A) returns() once()
        (system.onEvent _) expects(entity, B) returns() once()
      }

      // Start test
      processor.process
      processor.process
      processor.process
      processor.process
      processor.process

    }

    it ("should not send events to entities that doesn't match the system subscription") {

      Given("a system that subscribes to a component, and an entity without that component")
      val f = eventProcessor(true)
      import f._
      val entity = new ReusableEntity(1)
      val entityManager = mock[EntityComponentManager]
      processor.setEntityManager(entityManager)
      (entityManager.hasComponent[TestComponent] _) expects(entity, componentType) returns(false) anyNumberOfTimes()

      When("sending an event to the entity")
      val event = new TestEvent()
      event.id = 0
      processor.send(event, entity)

      Then("the system mock shouldn't be invoked")
      // Nothing happens
      processor.process

    }

    it ("should abort event processing if an event is cancelled") {

      Given("an event that reports itself as enabled the first time and disabled the second time")
      val event = mock[Event]
      (event.isCancelled _) expects() returns(false) once()
      (event.isCancelled _) expects() returns(true) once()
      (event.id _) expects() returns(0) anyNumberOfTimes()

      When("sending the event to be processed by two systems")
      val f = eventProcessor(true)
      import f._
      (entityManager.hasComponent[TestComponent] _) expects(*, *) returns(true) anyNumberOfTimes()
      val system2 = mock[EventSystem]
      systemBuffer += system2
      (system2.isEnabled _) expects() returns(true) anyNumberOfTimes()
      val entity = new ReusableEntity(1)
      processor.send(event, entity)

      Then("only system 1 should receive the event")
      (system.onEvent _) expects(entity, event) returns() once()
      // Nothing for system 2 mock

      // Start test
      processor.process should be (true)
      processor.process should be (false)

    }
  }
}
