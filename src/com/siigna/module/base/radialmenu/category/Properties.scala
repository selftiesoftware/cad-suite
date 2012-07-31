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

import com.siigna.app.model.shape.TextShape
import com.siigna.module.base.radialmenu._
import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D

case class Properties(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.propertiesColor

  def name = "Properties"

  override def NNE = Some(MenuItem('SampleProperties, RadialMenuIcon.sampleProperties, "properties"))
  override def N   = Some(CategoryItemEmpty("sampling"))
  //override def E   = Some(new MenuPreferences(Some(this)))
  //override def ESE = Some(MenuItem('Performancetest, RadialMenuIcon.test, "properties"))
  //override def ENE = Some(MenuItem('AntiAliasingToggle, TextShape("antiAlias", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  //override def WNW = Some(MenuItem('Polyline, TextShape("mm", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  //override def W   = Some(MenuItemEmpty("units"))
  //override def WSW = Some(MenuItem('Polyline, TextShape("Feet", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  //override def S   = Some(MenuItemEmpty("style"))
  //override def SSE = Some(MenuItem('Polyline, TextShape("line style", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  //override def SSW = Some(MenuItem('Polyline, TextShape("text style", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
}
