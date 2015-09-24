package entityComponent.miranda

import net.cyndeline.scalarlib.entityComponent.miranda._
import net.cyndeline.scalarlib.entityComponent.miranda.entityManager.EntityManager
import net.cyndeline.scalarlib.entityComponent.miranda.exceptions.InvalidEntityReferenceException
import testHelpers.SpecImports

import scala.collection.mutable.ListBuffer

class EntityManagerSpec extends SpecImports {



  def entityManager = new {
    val handlerMock = mock[EventHandlerSetup]
    (handlerMock.setSystemBuffer _) expects(*) returns() anyNumberOfTimes()
    (handlerMock.setEntityManager _) expects(*) returns() anyNumberOfTimes()
    val manager = new EntityManager(handlerMock)
  }

  describe("EntityManager") {

    /*
     * Adding/removing components
     */

    it ("should add components to entities") {

      Given("a manager with an entity")
      val manager = entityManager.manager
      val entity = manager.createEntity
      val componentType = manager.componentType(classOf[TestComponent])

      When("adding a component")
      val c: TestComponent = manager.addComponent(entity, componentType)

      Then("the entity should have the component")
      manager.hasComponent(entity, componentType) should be (true)
      val comp = manager.getComponent(entity, componentType)
      comp.someInt = 99
      val comp2 = manager.getComponent(entity, componentType)
      comp.someInt should be (comp2.someInt)

      And("the component returned at creation should be the component that was created")
      c should equal (comp)

    }

    it ("should remove components") {

      Given("a manager with an entity having a component")
      val manager = entityManager.manager
      val entity = manager.createEntity
      val componentType = manager.componentType(classOf[TestComponent])
      manager.addComponent(entity, componentType)
      manager.hasComponent(entity, componentType) should be (true)

      When("removing the component")
      manager.removeComponent(entity, componentType)

      Then("the entity should not have a component of the type that was removed")
      manager.hasComponent(entity, componentType) should be (false)

    }

    it ("should throw an exception when attempting to register abstract classes as components") {

      Given("a manager")
      val manager = entityManager.manager

      When("registering an abstract component")

      Then("an exception should be thrown")
      val thrown = intercept[IllegalArgumentException] {
        manager.componentType(classOf[AbstractTestComponent])
      }
      thrown.getMessage should be (classOf[AbstractTestComponent] + " is abstract and cannot be registered as a component.")

    }

    it ("should throw an exception when attempting to register a component class that lacks an empty constructor") {

      Given("a manager")
      val manager = entityManager.manager

      When("adding a component class without a default constructor to an assemblage")
      val c = classOf[ComponentWithoutDefaultConstructor]

      Then("an exception should be thrown")
      val thrown = intercept[IllegalArgumentException] {
        manager.componentType(c)
      }
      thrown.getMessage should be ("Cannot register components that lack a no-argument constructor: " + c.getSimpleName + ". Make sure the component is not an inner class.")

    }

    /*
     * Events
     */

    it ("should send events") {

      Given("a manager with an entity")
      val f = entityManager
      import f._
      val entity = manager.createEntity
      val event = new TestEvent()

      When("sending an event to an entity")
      Then("the event handler should receive the entity and the event")
      (handlerMock.send _) expects(event, entity) returns() once()

      // Start test
      manager.sendEvent(entity, event)

    }

    it ("should respond to events") {

      Given("a manager with an entity")
      val f = entityManager
      import f._
      val entity = manager.createEntity
      val event1 = new TestEvent()
      val event2 = new TestEvent()

      When("responding to an event")
      Then("the event handler should receive the entity and the event and the event being responded to")
      (handlerMock.send _) expects(event1, entity) returns() once()

      // Start test
      manager.replyToEvent(entity, event1, event2)

    }

    it ("should produce event factories") {

      Given("a manager")
      val manager = entityManager.manager

      When("producing an event factory")
      val fact = manager.eventFactory(classOf[TestEvent])

      Then("the factory should produce events")
      val event1: TestEvent = fact.createEvent
      val event2: TestEvent = fact.createEvent
      event1 should not equal (event2)

    }

    /*
     * Assemblages
     */

    it ("should assemble entities with empty class types") {

      Given("a manager")
      val manager = entityManager.manager
      manager.componentType(classOf[TestComponent])
      manager.componentType(classOf[TestComponent2])

      When("adding two components to an assembly")
      val assemblage = manager.registerAssemblage("Assemblage", "An assemblage with two components", Set(classOf[TestComponent], classOf[TestComponent2]), Set())

      Then("assembling a new entity should give it those components")
      val entityWithComponents = manager.assembleEntity(assemblage)
      val componentType1 = manager.componentType(classOf[TestComponent])
      val componentType2 = manager.componentType(classOf[TestComponent])

      manager.hasComponent(entityWithComponents, componentType1) should be (true)
      manager.hasComponent(entityWithComponents, componentType2) should be (true)

    }

    it ("should assemble entities using components with pre-set data") {

      Given("a manager and a component with two public vars")
      val manager = entityManager.manager
      val cType = manager.componentType(classOf[TestComponent])
      val dataComponent = new TestComponent()

      When("modifying the var data and registering the component to an assemblage")
      val intValue = 9999
      val stringValue = "A modified string"
      dataComponent.someInt = intValue
      dataComponent.someString = stringValue
      val assemblage = manager.registerAssemblage("Assemblage", "description", Set(), Set(dataComponent))

      Then("new entities should be created with the data component")
      val entity = manager.assembleEntity(assemblage)
      manager.hasComponent(entity, cType) should be (true)

      And("the component instance should not be the same as the one used to set data")
      val newComponent: TestComponent = manager.getComponent(entity, cType)
      newComponent should not equal (dataComponent)

      And("the data in the assembled component should equal the one in the data component")
      newComponent.someInt should equal (dataComponent.someInt)
      newComponent.someString should equal (dataComponent.someString)

      And("changing the values in the original component should not affect the new one")
      dataComponent.someInt = -1
      dataComponent.someString = ""
      newComponent.someInt should be (intValue)
      newComponent.someString should be (stringValue)

    }

    it ("should store component data in the scope of extended classes") {

      Given("a manager and a component that extends another component")
      val manager = entityManager.manager
      val dataComponent = new ExtendingComp()
      val cType = manager.componentType(classOf[ExtendingComp])

      When("modifying the var data and registering the component to an assemblage")
      dataComponent.extendedData = false
      dataComponent.someInt = 88
      val assemblage = manager.registerAssemblage("Assemblage", "description", Set(), Set(dataComponent))

      Then("new entities should be created with the data component")
      val entity = manager.assembleEntity(assemblage)
      manager.hasComponent(entity, cType) should be (true)

      And("the data in the component should be the same as the input component")
      val newComponent = manager.getComponent(entity, cType)
      newComponent.extendedData should be (false)
      newComponent.someInt should be (88)

    }

    it ("should invoke clone() on the components public var datastructures that implements the scala Cloneable trait") {

      Given("a manager and a component with a public list extending scalas Clonable trait")
      val manager = entityManager.manager
      val component = new ComponentWithScalaCloneData()
      val cType = manager.componentType(classOf[ComponentWithScalaCloneData])

      When("modifying the data and registering the component to an assemblage")
      val data = "Some data"
      component.clonableVarList += data
      val assemblage = manager.registerAssemblage("Assemblage", "description", Set(), Set(component))

      Then("entities built from the assemblage should all have the modified data")
      val expectedData = new ListBuffer[String]()
      expectedData += data
      val e1 = manager.assembleEntity(assemblage)
      val e2 = manager.assembleEntity(assemblage)
      val e1Component = manager.getComponent(e1, cType)
      val e2Component = manager.getComponent(e2, cType)
      e1Component.clonableVarList should be (expectedData)
      e2Component.clonableVarList should be (expectedData)

      And("the datastructure instances should all be unique")
      assert(!e1Component.clonableVarList.eq(component.clonableVarList))
      assert(!e2Component.clonableVarList.eq(component.clonableVarList))
      assert(!e1Component.clonableVarList.eq(e2Component.clonableVarList))

    }

    it ("should invoke clone() on the components private var datastructures that implements the scala Cloneable trait") {

      Given("a manager and a component with a private list extending scalas Clonable trait")
      val manager = entityManager.manager
      val component = new ComponentWithScalaCloneData()
      val cType = manager.componentType(classOf[ComponentWithScalaCloneData])

      When("modifying the data and registering the component to an assemblage")
      val data = "Some data"
      component.addDataToPrivateList(data)
      val assemblage = manager.registerAssemblage("Assemblage", "description", Set(), Set(component))

      Then("entities built from the assemblage should all have the modified data")
      val expectedData = new ListBuffer[String]()
      expectedData += data
      val e1 = manager.assembleEntity(assemblage)
      val e2 = manager.assembleEntity(assemblage)
      val e1Component = manager.getComponent(e1, cType)
      val e2Component = manager.getComponent(e2, cType)
      e1Component.getPrivateList should be (expectedData)
      e2Component.getPrivateList should be (expectedData)

      And("the datastructure instances should all be unique")
      assert(!e1Component.clonableVarList.eq(component.clonableVarList))
      assert(!e2Component.clonableVarList.eq(component.clonableVarList))
      assert(!e1Component.clonableVarList.eq(e2Component.clonableVarList))

    }

    it ("should throw an exception when attempting to register both an empty component class and a data object of the same type") {

      Given("a manager")
      val manager = entityManager.manager

      When("adding a component class and an instantiated version of the same component to the same assemblage")
      Then("an exception should be thrown")
      val instantiatedComponent = new TestComponent()
      intercept[IllegalArgumentException] {
        manager.registerAssemblage("Assemblage", "An assemblage with two components", Set(classOf[TestComponent]), Set(instantiatedComponent))
      }

    }

    it ("should throw an exception when attempting to add two data components of the same class to an assemblage") {

      Given("a manager")
      val manager = entityManager.manager

      When("adding two instantiated data components of the same class to an assemblage")
      val instantiatedComponent1 = new TestComponent()
      val instantiatedComponent2 = new TestComponent()

      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        manager.registerAssemblage("Assemblage", "An assemblage with two components", Set(), Set(instantiatedComponent1, instantiatedComponent2))
      }

    }

    it ("should throw an exception when attempting to register a class that hasn't been assigned a component type yet using its class type") {

      Given("a manager")
      val manager = entityManager.manager

      When("registering an assemblage using a component type that hasn't been registered before")
      val missingClass = classOf[TestComponent]

      Then("an exception should be thrown")
      val thrown = intercept[IllegalArgumentException] {
        manager.registerAssemblage("Assemblage", "Missing class", Set(missingClass), Set())
      }
      thrown.getMessage should be ("requirement failed: The component class " + missingClass.getSimpleName + " hasn't been registered yet. A component is registered when its Type object is retrieved for the first time.")

    }

    it ("should throw an exception when attempting to register a class that hasn't been assigned a component type yet using a data object") {

      Given("a manager")
      val manager = entityManager.manager

      When("registering an assemblage using an instantiated component that hasn't had its component type registered before")
      val createdComponent = new TestComponent()

      Then("an exception should be thrown")
      val thrown = intercept[IllegalArgumentException] {
        manager.registerAssemblage("Assemblage", "Missing class", Set(), Set(createdComponent))
      }
      thrown.getMessage should be ("requirement failed: The component class " + createdComponent.getClass.getSimpleName + " hasn't been registered yet. A component is registered when its Type object is retrieved for the first time.")

    }

    /*
     * Blueprints
     */

    it ("should increment blueprint ID's") {

      Given("a manager")
      val manager = entityManager.manager

      When("creating three blueprints")
      val bp1 = manager.registerBlueprint("BP1", "BP1")
      val bp2 = manager.registerBlueprint("BP2", "BP2")
      val bp3 = manager.registerBlueprint("BP3", "BP3")

      Then("the blueprints should have ID 0, 1 and 2")
      bp1.id should be (0)
      bp2.id should be (1)
      bp3.id should be (2)

    }

    it ("should create entities using the assemblages specified in a blueprint") {

      Given("a blueprint with two entities specified as E1 and E2")
      val manager = entityManager.manager
      manager.componentType(classOf[TestComponent2])
      val dataComponent = new TestComponent2()
      dataComponent.someDouble = 3.0
      val assemblageE1 = manager.registerAssemblage("E1", "E1", Set(classOf[TestComponent2]), Set())
      val assemblageE2 = manager.registerAssemblage("E2", "E2", Set(), Set(dataComponent))
      val blueprint = manager.registerBlueprint("E1 BP", "BP description").assignEntityKey("E1", assemblageE1).assignEntityKey("E2", assemblageE2)

      When("constructing entities using the blueprint")
      val entityMap = manager.assembleFromBlueprint(blueprint)

      Then("the map should contain entries for E1 and E2")
      entityMap.keySet should contain ("E1")
      entityMap.keySet should contain ("E2")

      And("the entity E1 should contain the specified component with default data")
      val cType = manager.componentType(classOf[TestComponent2])
      val e1Component = manager.getComponent(entityMap("E1"), cType)
      e1Component.someDouble should be (2.0)

      And("the entity E2 should contain the specified component with modified data")
      val e2Component = manager.getComponent(entityMap("E2"), cType)
      e2Component.someDouble should be (3.0)

    }

    it ("should send entities to blueprints in the order that the blueprints key arguments occur") {

      Given("a blueprint with a processor that takes the key arguments E1 and E2 in that order")
      val processor = new TestBlueprintProcessor("E1", "E2")
      val manager = entityManager.manager
      val assemblageE1 = manager.registerAssemblage("E1", "E1", Set(), Set())
      val assemblageE2 = manager.registerAssemblage("E2", "E2", Set(), Set())
      val blueprint = manager.registerBlueprint("E1 BP", "BP description")
        .assignEntityKey("E1", assemblageE1)
        .assignEntityKey("E2", assemblageE2)
        .registerEntityProcessor(processor)

      When("constructing entities using the blueprint")
      val entityMap = manager.assembleFromBlueprint(blueprint)

      Then("the processor should receive its entities in the order E1 -> E2")
      processor.processedEntities should be (Vector(entityMap("E1"), entityMap("E2")))

    }

    /*
     * Deleted entities
     */

    it ("should throw an error when attempting to use a deleted entity to retrieve components") {

      Given("a manager and an entity with a component")
      val manager = entityManager.manager
      val entity = manager.createEntity
      val componentType = manager.componentType(classOf[TestComponent])
      manager.addComponent(entity, componentType)

      When("deleting the entity and creating another one with the reused id")
      manager.deleteEntity(entity)
      val reusedIdEntity = manager.createEntity
      manager.addComponent(reusedIdEntity, componentType)

      reusedIdEntity.id should equal (entity.id)

      Then("using that entity to retrieve components should result in an error")
      intercept[InvalidEntityReferenceException] {
        manager.getComponent(entity, componentType)
      }

    }

    it ("should throw an exception when attempting to use a deleted entity to add components") {

      Given("a manager and an entity with a component")
      val manager = entityManager.manager
      val entity = manager.createEntity
      val componentType1 = manager.componentType(classOf[TestComponent])
      manager.addComponent(entity, componentType1)

      When("deleting the entity and creating another one with the reused id")
      manager.deleteEntity(entity)
      val reusedIdEntity = manager.createEntity
      manager.addComponent(reusedIdEntity, componentType1)

      reusedIdEntity.id should equal (entity.id)

      Then("using that entity to add components should result in an error")
      intercept[InvalidEntityReferenceException] {
        val componentType2 = manager.componentType(classOf[TestComponent2])
        manager.addComponent(entity, componentType2)
      }

    }

    it ("should throw an exception when attempting to use a deleted entity to remove components") {

      Given("a manager and an entity with a component")
      val manager = entityManager.manager
      val entity = manager.createEntity
      val componentType1 = manager.componentType(classOf[TestComponent])
      manager.addComponent(entity, componentType1)

      When("deleting the entity and creating another one with the reused id")
      manager.deleteEntity(entity)
      val reusedIdEntity = manager.createEntity
      manager.addComponent(reusedIdEntity, componentType1)

      reusedIdEntity.id should equal (entity.id)

      Then("using that entity to remove components should result in an error")
      intercept[InvalidEntityReferenceException] {
        val componentType2 = manager.componentType(classOf[TestComponent2])
        manager.removeComponent(entity, componentType2)
      }

    }

    it ("should throw an error when attempting to use a deleted entity to check existence of components") {

      Given("a manager and an entity")
      val manager = entityManager.manager
      val entity = manager.createEntity
      val componentType = manager.componentType(classOf[TestComponent])

      When("deleting the entity and creating another one with the reused id")
      manager.deleteEntity(entity)
      val reusedIdEntity = manager.createEntity
      manager.addComponent(reusedIdEntity, componentType)

      reusedIdEntity.id should equal (entity.id)

      Then("using that entity to check component presence should result in an error")
      intercept[InvalidEntityReferenceException] {
        manager.hasComponent(entity, componentType)
      }

    }

    it ("should throw an exception when attempting to delete an already deleted entity") {

      Given("a manager and an entity")
      val manager = entityManager.manager
      val entity = manager.createEntity

      When("deleting the entity twice")
      Then("an exception should be thrown")
      intercept[InvalidEntityReferenceException] {
        manager.deleteEntity(entity)
        manager.deleteEntity(entity)
      }

    }

    it ("it should throw an exception when attempting to send a message to a deleted entity") {

      Given("a manager and an entity")
      val manager = entityManager.manager
      val entity = manager.createEntity

      When("deleting the entity")
      manager.deleteEntity(entity)

      Then("sending messages to it should result in an exception")
      val event = new TestEvent()
      intercept[InvalidEntityReferenceException] {
        manager.sendEvent(entity, event)
      }

    }

    it ("it should throw an exception when attempting to respond to a message to a deleted entity") {

      Given("a manager and an entity")
      val manager = entityManager.manager
      val entity = manager.createEntity

      When("deleting the entity")
      manager.deleteEntity(entity)

      Then("sending responses to it should result in an exception")
      val event1 = new TestEvent()
      val event2 = new TestEvent()
      intercept[InvalidEntityReferenceException] {
        manager.replyToEvent(entity, event1, event2)
      }

    }

    /*
     * Listeners
     */

    it ("should invoke the listener whenever an entity is created and deleted") {

      Given("a manager and an entity listener")
      val manager = entityManager.manager
      val listener = mock[EntityListener]

      When("adding a listener")
      manager.addEntityListener(listener)

      Then("the listener should call onCreate when an entity is created")
      (listener.onCreate _) expects(*) returns() once()
      val entity = manager.createEntity

      And("the listener should call onDelete when an entity is deleted")
      (listener.onDelete _) expects(*) returns() once()
      manager.deleteEntity(entity)

    }

    it ("should allow an entity passed into a listener to be used in queries") {

      Given("a manager and an entity listener that checks for component existence on entities passed to it")
      val manager = entityManager.manager
      val listener = new TestListener(manager)

      When("adding a listener")
      manager.addEntityListener(listener)

      Then("creating and deleting entities should be possible without exception being thrown from dead entity refs")
      val entity = manager.createEntity
      manager.deleteEntity(entity)

    }

    it ("should invoke multiple entity listeners") {

      Given("a manager and two entity listeners")
      val manager = entityManager.manager
      val listener1 = mock[EntityListener]
      val listener2 = mock[EntityListener]

      When("adding multiple listeners")
      manager.addEntityListener(listener1)
      manager.addEntityListener(listener2)

      Then("they should all be invoked")
      (listener1.onCreate _) expects(*) returns() once()
      (listener2.onCreate _) expects(*) returns() once()

      manager.createEntity // Start test

    }

    it ("should invoke entity listeners when assembling entities") {

      Given("a manager with an entity listener")
      val manager = entityManager.manager
      manager.componentType(classOf[TestComponent2])
      val listener = mock[EntityListener]
      manager.addEntityListener(listener)

      When("assembling an entity")
      val assemblage = manager.registerAssemblage("Assemblage", "description", Set(classOf[TestComponent2]), Set())

      Then("the listener should be invoked")
      (listener.onCreate _) expects(*) returns() once()

      // Start test
      manager.assembleEntity(assemblage)

    }

    it ("should add components from an assemblage before calling the entity listeners") {

      Given("a manager with an entity listener that checks for the component TestComponent")
      val manager = entityManager.manager
      val componentType = manager.componentType(classOf[TestComponent])
      val listener = new EntityListener() {
        def onCreate(entity: Entity): Unit = {
          assert(manager.hasComponent(entity, componentType), "Component " + classOf[TestComponent] + " not found.")
        }
        def onDelete(entity: Entity): Unit = ???
      }
      manager.addEntityListener(listener)

      When("assembling an entity with the component TestComponent")
      val assemblage = manager.registerAssemblage("Assemblage", "description", Set(classOf[TestComponent]), Set())

      Then("the listener assertion should validate")

      // Start test
      manager.assembleEntity(assemblage)

    }

  }
}

/**
 * Listens to events then calls a method on the entity to make sure its eligible for querying.
 */
class TestListener(manager: EntityManager) extends EntityListener {
  private val cType = manager.componentType(classOf[TestComponent])
  def onCreate(entity: Entity): Unit = {
    manager.hasComponent(entity, cType)
  }

  def onDelete(entity: Entity): Unit = {
    manager.hasComponent(entity, cType)
  }
}

class TestComponent() extends Component {
  var someInt = 1
  var someString = "Test"
}

class TestComponent2() extends Component {
  var someDouble = 2.0
}

abstract class AbstractTestComponent() extends Component {
  val i: Int
}

class ComponentWithoutDefaultConstructor(val param: Int) extends Component {
  val p = param
}

class ComponentWithScalaCloneData extends Component {
  var clonableVarList = new ListBuffer[String]()
  val clonableValList = new ListBuffer[String]()

  def addDataToPrivateList(d: String) {
    privateVarList += d
  }
  def getPrivateList = privateVarList
  private var privateVarList = new ListBuffer[String]()
}

class ExtendingComp extends TestComponent {
  var extendedData: Boolean = true

  private var m = -100
  def method: Int = -1
  def method_=(newInt: Int) = m = newInt
}

class TestEvent extends Event

class TestBlueprintProcessor(args: String*) extends BlueprintProcessor(args:_*) {
  var processedEntities = Vector[Entity]()

  def processEntities(entities: Seq[Entity]): Unit = {
    processedEntities = entities.toVector
  }
}
