package net.cyndeline.scalarlib.entityComponent.miranda.exceptions

/**
 * A killed entity has been used to modify or retrieve data in the system
 */
class InvalidEntityReferenceException(id: Int)
  extends RuntimeException("The entity id " + id + " has already been deleted. Since its identifier may have been reused in another entity, the new entity instance must be used when accessing it.") {

}
