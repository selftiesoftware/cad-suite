/*
 * Copyright (c) 2012. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.base.radialmenu

import com.siigna.app.model.shape.Shape

import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D
import com.siigna.app.model.shape.TextShape

/**
 * A MenuItem is an item in the [[RadialMenu]].
 * 
 * @param module  The symbolic name of the module.
 * @param icon  The icon to be displayed in the menu, given as any number of shapes.
 * @param modulePath  The path where the module can be found and loaded.
 */
class MenuItem(val module : Symbol, val icon : Traversable[Shape], val modulePath : String) extends MenuElement


/**
 * A companion object to the MenuItem allowing simple creation.
 */
object MenuItem {

  def apply(module : Symbol, icon : Shape) : MenuItem = MenuItem(module, Traversable[Shape](icon))
  def apply(module : Symbol, icon : Shape, modulePath : String) : MenuItem =new MenuItem(module, Traversable[Shape](icon), "com.siigna.module.base." + modulePath)
  def apply(module : Symbol, icon : Traversable[Shape]) = new MenuItem(module, icon, "com.siigna.module.base." + module.name)
  def apply(module : Symbol, icon : Traversable[Shape], modulePath : String) = {
    if (modulePath.startsWith("com.siigna")) { // Don't write the path twice
      new MenuItem(module, icon, modulePath)
    } else { // If the "com.siigna" part hasn't been defined, assume it's in com.siigna.module
      // TODO: Remove ".base" from the path - we need to make this module independent
      new MenuItem(module, icon, "com.siigna.module.base." + modulePath)
    }
  }

}

case class MenuItemEmpty(text : String) extends MenuItem('None, Iterable(TextShape(text, Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))), "")
