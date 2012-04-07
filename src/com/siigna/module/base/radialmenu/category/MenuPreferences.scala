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

package com.siigna.module.base.radialmenu.category

import com.siigna.module.base.radialmenu._
import com.siigna.app.model.shape.TextShape
import com.siigna.module.base.radialmenu._
import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D

case class MenuPreferences (parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.propertiesColor

  def name = "Preferences"

  override def N = Some(MenuItemEmpty("Selection"))
  override def E = Some(MenuItemEmpty("Paper"))
  override def S = Some(MenuItemEmpty("Display"))
  override def W = Some(MenuItemEmpty("Navigation"))

  override def NNE = Some(MenuItem('PreferencesSet, TextShape("sel dist", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  override def NNW = Some(MenuItem('PreferencesSet, TextShape("sel color", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  override def ENE = Some(MenuItem('PreferencesSet, TextShape("paper size", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  override def ESE = Some(MenuItem('PreferencesSet, TextShape("paper color", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  override def SSE = Some(MenuItem('PreferencesSet, TextShape("backgr color", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  override def SSW = Some(MenuItem('AntiAliasingToggle, TextShape("anti-alias", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  override def WNW = Some(MenuItem('PreferencesSet, TextShape("zoom speed", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))


  //override def NNW = Some(MenuItem('Arc, RadialMenuIcon.arc , "create"))

}