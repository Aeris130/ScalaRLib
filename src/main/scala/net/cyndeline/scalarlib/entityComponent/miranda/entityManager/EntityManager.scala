package net.cyndeline.scalarlib.entityComponent.miranda.entityManager

import net.cyndeline.scalarlib.entityComponent.miranda.util.Bag
import net.cyndeline.scalarlib.entityComponent.miranda.entityManager.componentCreation.{ComponentVarIterator, ComponentFactoryFactory, ComponentFactory}
import scala.collection.mutable.ArrayBuffer
import net.cyndeline.scalarlib.entityComponent.miranda._
import net.cyndeline.scalarlib.entityComponent.miranda.entityManager.eventCreation.{EventFactoryFactory, EventFactory}
import net.cyndeline.scalarlib.entityComponent.miranda.eventProcessing.EventReplyOrderProcessor
import net.cyndeline.scalarlib.entityComponent.miranda.exceptions.InvalidEntityReferenceException
import scala.reflect.runtime.universe._
import net.cyndeline.scalarlib.util.IDPool
import scala.collection.mutable.{ Cloneable => ScalaCloneable }

/**
 * Interfaces with the user by storing and retrieving entity-component data.
 */
class EntityManager(handler: EventHandlerSetup) extends EntityComponentManager {
  private var entityIdPdPool =    new IDPool() //TODO Persist free ids when saving the game to file
  private var componentIdPool =   new IDPool()
  private var eventIdPool =       new IDPool()
  private var assemblageIdPool =  new IDPool()
  private var blueprintIdPool =   new IDPool()

  private val componentTable = new ComponentTable()
  private var componentKeys = Map[Class[_ <: Component], ComponentType[_]]()

  /* Contains factory objects for every component, where the index of the factory corresponds
   * to the id of the component. Can be used to check if a component id has been registered
   * (otherwise the index is empty).
   */
  private val componentFactories = new Bag[ComponentFactory]()
  private val factoryCreation = new ComponentFactoryFactory()

  /* Event factories that the user may request for systems that send events. */
  private var eventFactories = Map[Class[_ <: Event], EventFactory[_]]()
  private val eventFactoryCreation = new EventFactoryFactory()
  private var eventIds = Map[Class[_ <: Event], Int]()

  /* Allows the manager to retrieve all systems that subscribe to an event at constant time.
   * The index in the bag denotes an event id, and the buffer of systems at that index
   * contains every system subscribed to the event.
   */
  private val systemEventSubscriptions = new Bag[ArrayBuffer[EventSystem]]()
  handler.setSystemBuffer(systemEventSubscriptions)
  handler.setEntityManager(this)

  /* Stores component sets to use when creating entities. An index in the bag corresponds to the
   * assemblage id needed to assemble it.
   */
  private val assemblages = new Bag[ArrayBuffer[AssemblyData]]()
  private val dataParser = new ComponentVarIterator() // Accesses all public vars with reflection
  private val blueprints = new Bag[Blueprint]()

  /* Each container resides in the index that corresponds to the entity id. */
  private val entityContainers = new Bag[Entity]()

  /* Notified in O(n) time whenever entity data changes where n is the amount of listeners. */
  private var entityListeners = new ArrayBuffer[EntityListener]()

  /**
   * Constructs a new entity manager.
   * @return A new entity manager.
   */
  def this() = this(new EventReplyOrderProcessor())

  /**
   * Creates a new entity entry.
   * @return Id of the entity that was just created.
   */
  override def createEntity: Entity = createEntity(true)

  /** Deletes an entity and frees up its id for re-use. */
  override def deleteEntity(entity: Entity) {
    checkIfAlive(entity)
    if (!entityListeners.isEmpty)
      listenersOnDelete(entity)

    componentTable.unregisterEntity(entity.id)
    entityIdPdPool -= entity.id
    entityContainers.remove(entity.id)
    entity.asInstanceOf[ReusableEntity].kill()
  }

  /** Assembles an entity with a predetermined set of components. */
  override def assembleEntity(assemblage: Assemblage): Entity = {
    val componentsToAdd = assemblages.get(assemblage.id).getOrElse {
      throw new Error("The assemblage \"" + assemblage.name + "\" could not be assembled, no component data found in manager for id " + assemblage.id + ".")
    }.toIterator

    val entity = createEntity(false)
    while (componentsToAdd.hasNext) {
      val componentAndData: AssemblyData = componentsToAdd.next()
      val component = addComponent(entity, componentAndData.cType)

      if (componentAndData.data.isDefined) {
        val varNamesAndData: Map[String, Any] = componentAndData.data.get

        // Function that stores data in a field based on its name
        def setVarData(fieldName: String, field: FieldMirror) {

          val d = varNamesAndData.get(fieldName).getOrElse {
            throw new Error("No public accessor with the name [" + fieldName + "] found in component type " + component.getClass)
          }

          val valueToAdd = d match {
            case c: ScalaCloneable[_] => c.clone()
            case _ => d
          }

          field.set(valueToAdd)
        }
        // End function

        dataParser.iterate(component, setVarData)
      }
    }
    listenersOnCreate(entity)
    entity
  }

  /** Assembles one or more entities based on a blueprint where each such entity is mapped to a String key. */
  override def assembleFromBlueprint(blueprint: Blueprint): Map[String, Entity] = {
    val createdEntities = blueprint.entityKeys.map(kv => kv._1 -> assembleEntity(kv._2))

    for (p <- blueprint.processors) {
      val args: Seq[String] = p.arguments // Needed, or the compiler thinks the map wants to iterate over chars
      val entitySeq = args.map(createdEntities)
      p.processEntities(entitySeq)
    }

    createdEntities
  }

  /**
   * Adds a component with default data to an entity.
   * @return The component that was added.
   */
  override def addComponent[T <: Component](entity: Entity, key: ComponentType[T]): T = {
    checkIfAlive(entity)
    val factory = componentFactories.get(key.componentId).getOrElse {
      throw new Error("Couldn't find factory for component " + key.name)
    }
    val c = factory.produceComponent
    componentTable.addComponent(entity.id, key.componentId, c)
    c.asInstanceOf[T]
  }

  /** Removes a component from an entity. */
  override def removeComponent[T <: Component](entity: Entity, key: ComponentType[T]) {
    checkIfAlive(entity)
    componentTable.removeComponent(entity.id, key)
  }

  /** Retrieves a component from an entity, of the type specified by the component key. */
  override def getComponent[T <: Component](entity: Entity, key: ComponentType[T]): T = {
    checkIfAlive(entity)
    componentTable.getComponent(entity.id, key.componentId).asInstanceOf[T]
  }

  /** Checks if a component is present in an entity. */
  override def hasComponent[T <: Component](entity: Entity, key: ComponentType[T]): Boolean = {
    checkIfAlive(entity)
    componentTable.hasComponent(entity.id, key.componentId)
  }

  /** Sends an event to an entity. */
  override def sendEvent(entity: Entity, event: Event) {
    checkIfAlive(entity)
    handler.send(event, entity)
  }

  /** Sends an event to the event handler as a response to another event. */
  override def replyToEvent(entity: Entity, eventToReplyWith: Event, eventToReplyTo: Event) {
    checkIfAlive(entity)
    eventToReplyWith.replyOrder = eventToReplyTo.replyOrder + 1
    handler.send(eventToReplyWith, entity)
  }

  /**
   * Retrieves a key mapped to a specific component type. Do not call this method every time a key is needed,
   * as the lookup time isn't constant.
   * @param componentClass Class to retrieve key for.
   * @return A component key belonging to the specified class.
   */
  override def componentType[T <: Component : TypeTag](componentClass: Class[T]): ComponentType[T] = {
    componentKeys.get(componentClass).getOrElse {

      // Abstract classes are not allowed for obvious reasons
      val m = runtimeMirror(getClass.getClassLoader)
      val cSym = m.classSymbol(componentClass)
      if (cSym.asClass.isAbstractClass)
        throw new IllegalArgumentException(componentClass + " is abstract and cannot be registered as a component.")

      val newComponentId = componentIdPool.nextId
      componentIdPool = newComponentId._2

      // register new id
      val newKey = new ComponentType[T](newComponentId._1, componentClass.getSimpleName)
      componentKeys += (componentClass -> newKey)

      // Register new factory
      val compFactory = factoryCreation.buildFactory(componentClass)
      componentFactories.set(newComponentId._1, compFactory)

      newKey
    }.asInstanceOf[ComponentType[T]]
  }

  /**
   * Retrieves a factory used to instantiate events of a particular class. Looking up factories is not done
   * at constant time, do not invoke this method every time an event must be created, or the performance gains
   * from using the factory will be lost.
   *
   * @param eventClass Class to instantiate. Must have an empty constructor.
   * @tparam E Event type.
   * @return A factory that instantiates events of the specified type with default data.
   */
  override def eventFactory[E <: Event](eventClass: Class[E]): EventFactory[E] = {
    val fact = eventFactories.get(eventClass).getOrElse {
      registerNewEvent(eventClass)
      eventFactories(eventClass)
    }

    fact.asInstanceOf[EventFactory[E]]
  }

  /** Subscribes a system to the set of events in its subscription. */
  override def registerSystem(system: EventSystem) {
    for (event <- system.subscription.events) {
      val id = eventIds.get(event).getOrElse {
        registerNewEvent(event)
        eventIds(event)
      }

      val currentSystems = getSystemSubscriptions(id)
      currentSystems += system
    }

    /* Add all components to the systems subscriptions, and register them if this is the first time the component has
     * been referenced.
     */
    system.subscription.includeAllTypes = for (includedComponent <- system.subscription.includeAll) yield componentType(includedComponent)
  }

  /**
   * Registers a set of components to be added to a newly created entity whenever the resulting assembler object
   * is supplied. Call only once per assemblage object, or duplicates will be created.
   */
  override def registerAssemblage(name: String, description: String, componentSet: Set[Class[_ <: Component]], dataSet: Set[Component]): Assemblage = {
    val assemblyData = new ArrayBuffer[AssemblyData]()
    val assemblageId = assemblageIdPool.nextId
    assemblageIdPool = assemblageId._2

    /* Add every empty component class. */
    for (componentClass <- componentSet) {
      checkIfRegistered(componentClass)
      assemblyData += new AssemblyData(componentType(componentClass)) // registers type if it doesn't exist yet
    }

    /* Register the data found in the component objects. */
    for (componentData <- dataSet) {
      checkIfRegistered(componentData.getClass)
      var dataMap = Map[String, Any]()
      val typeOfComponent = componentType(componentData.getClass)

      if (assemblyData.exists(data => data.cType == typeOfComponent)) {
        throw new IllegalArgumentException("The component " + componentData.getClass + " is defined twice.")
      }

      // Iterate over every public var and add its data to the map
      def parseVars(nameOfVar: String, field: FieldMirror) {
        dataMap += (nameOfVar -> field.get)
      }
      dataParser.iterate(componentData, parseVars)
      assemblyData += new AssemblyData(typeOfComponent, dataMap)
    }

    assemblages.set(assemblageId._1, assemblyData)
    Assemblage(name, description, assemblageId._1)
  }

  /**
   * Instantiates a new blueprint. Additional blueprint data can be added to the returned object.
   */
  def registerBlueprint(name: String, description: String): Blueprint = {
    val id = blueprintIdPool.nextId
    blueprintIdPool = id._2
    val bp = new Blueprint(name, description, id._1)
    blueprints.set(id._1, bp)
    bp
  }

  /**
   * Adds a listener to the manager that is invoked upon data modification.
   */
  override def addEntityListener(listener: EntityListener) {
    entityListeners += listener
  }

  /**
   * Retrieves an event handler that processes events sent using this manager.
   * @return An event handler that can be used to process messages sent using this manager.
   */
  override def eventHandler: EventHandler = handler

  /* Creates an entity and calls creation listeners on it if specified. */
  private def createEntity(listenersShouldBeCalled: Boolean): Entity = {
    val entityId = entityIdPdPool.nextId
    entityIdPdPool = entityId._2
    componentTable.registerEntity(entityId._1)
    val newEntity = new ReusableEntity(entityId._1, this)
    entityContainers.set(entityId._1, newEntity)

    if (listenersShouldBeCalled) {
      listenersOnCreate(newEntity)
    }

    newEntity
  }

  private def registerNewEvent[E <: Event](c: Class[E]) {
    val eId = eventIdPool.nextId
    eventIdPool = eId._2
    eventIds += (c -> eId._1)
    val factory = eventFactoryCreation.buildFactory(c, eId._1)
    eventFactories += (c -> factory)
  }

  private def getSystemSubscriptions(eventId: Int): ArrayBuffer[EventSystem] = {
    val systemBuffer = systemEventSubscriptions.get(eventId)
    if (!systemBuffer.isDefined) {
      val newBuffer = new ArrayBuffer[EventSystem]()
      systemEventSubscriptions.set(eventId, newBuffer)
      newBuffer
    } else {
      systemBuffer.get
    }
  }

  private def checkIfAlive(entity: Entity): Boolean = {
    if (!entity.asInstanceOf[ReusableEntity].alive)
      throw new InvalidEntityReferenceException(entity.id)

    true
  }

  private def listenersOnCreate(entity: Entity) {
    if (entityListeners.isEmpty) return

    var i = 0
    while (i < entityListeners.size) {
      entityListeners(i).onCreate(entity)
      i += 1
    }
  }

  private def listenersOnDelete(entity: Entity) {
    var i = 0
    while (i < entityListeners.size) {
      entityListeners(i).onDelete(entity)
      i += 1
    }
  }

  private def checkIfRegistered(c: Class[_ <: Component]) {
    require(componentKeys.contains(c), "The component class " + c.getSimpleName + " hasn't been registered yet. A component is registered when its Type object is retrieved for the first time.")
  }

  /**
   * Stores data used to assemble entities with a pre-set set of components.
   * @param cType Type of component to add.
   * @param data Map with the names of all public vars, mapped against the value to set in them using reflection.
   */
  private class AssemblyData(val cType: ComponentType[_ <: Component], val data: Option[Map[String, Any]]) {
    def this(comp: ComponentType[_ <: Component]) = this(comp, None)
    def this(comp: ComponentType[_ <: Component], data: Map[String, Any]) = this(comp, Option(data))
  }
}
