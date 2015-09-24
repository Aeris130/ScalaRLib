package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Combines multiple assemblages, and allows the user to specify factory objects that takes specific entities as input.
 * This can be used when one entity (item) should be added to the component of another (character) after creation.
 *
 * Each entity is assigned its own String key which must be used when referencing it.
 * @param name Name of the blueprint. For debugging only.
 * @param description A short description of the blueprint. For debugging only.
 */
protected class Blueprint (val name: String,
                           val description: String,
                           val entityKeys: Map[String, Assemblage],
                           val processors: Vector[BlueprintProcessor], val id: Int) {

  def this(name: String, description: String, id: Int) = this(name, description, Map(), Vector(), id)

  /**
   * Map an entity assemblage to the string key that the entity should be associated with once it has been assembled.
   * @param key A unique key for this blueprint. A single assemblage can be assigned multiple keys (this results in
   *            multiple entities being created using the same assemblage), but only one instance of a key may be
   *            assigned to an assemblage.
   * @param assemblage Assemblage used to create entity.
   * @return A copy of this blueprint with the key -> assemblage mapping added.
   */
  final def assignEntityKey(key: String, assemblage: Assemblage): Blueprint = {
    require(!entityKeys.keySet.contains(key), "Duplicate key registered: " + key)

    new Blueprint(name, description, entityKeys + (key -> assemblage), processors, id)
  }

  /**
   * Assigns the blueprint a processor to be called once the entities have been assembled.
   * @param bpps A set of processors to add to this blueprint. May only contain keys that have previously been
   *             registered.
   * @return A copy of this blueprint with the processors added.
   */
  final def registerEntityProcessor(bpps: BlueprintProcessor*): Blueprint = {
    for (p <- processors) validateProcessorArguments(p.arguments:_*)

    new Blueprint(name, description, entityKeys, processors ++ bpps, id)
  }

  private def validateProcessorArguments(arg: String*) {
    for (s <- arg)
      require(entityKeys.keySet.contains(s), "Attempted to register a Blueprint process using an entity key that has not yet been registered in the Blueprint: " + s)
  }


}

