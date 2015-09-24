package net.cyndeline.scalarlib.subcut

import com.escalatesoft.subcut.inject.NewBindingModule
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{SuperNodeFactory, SuperNodeFactoryInterface}

/**
 * Dependency bindings for the RLDungeon project.
 */
object ProjectConfiguration extends NewBindingModule(module => {
  import module._

  bind [SuperNodeFactoryInterface] toProvider { new SuperNodeFactory() }

})
