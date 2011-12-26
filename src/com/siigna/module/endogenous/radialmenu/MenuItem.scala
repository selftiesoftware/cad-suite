/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu

import com.siigna.app.model.shape.Shape

import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D
import com.siigna.app.model.shape.TextShape

case class MenuItem(module : Symbol, icon : Iterable[Shape]) extends MenuElement

object MenuItem {
  def apply(module : Symbol, icon : Shape) = new MenuItem(module, Iterable[Shape](icon))
}

case class MenuItemEmpty(text : String) extends MenuItem('None, Iterable(TextShape(text, Vector2D(0, 0), 10, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
