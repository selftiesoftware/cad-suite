/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu

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
  def apply(module : Symbol, icon : Traversable[Shape]) = new MenuItem(module, icon, "com.siigna.module.endogenous." + module.name)
  def apply(module : Symbol, icon : Traversable[Shape], modulePath : String) = new MenuItem(module, icon, "com.siigna.module.endogenous." + modulePath)

}

case class MenuItemEmpty(text : String) extends MenuItem('None, Iterable(TextShape(text, Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))), "")
