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

  override def E   = Some(new MenuPreferences(Some(this)))
  override def ESE = Some(MenuItem('Performancetest, RadialMenuIcon.test, "properties"))
  override def ENE = Some(MenuItem('AntiAliasingToggle, TextShape("anti-alias", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "properties"))
  override def N   = Some(MenuItemEmpty("style"))
  override def NNE = Some(MenuItem('Polyline, RadialMenuIcon.NNEArrow))
  override def NNW = Some(MenuItem('Polyline, RadialMenuIcon.NNWArrow))
  override def WNW = Some(MenuItem('Polyline, TextShape("mm", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def W   = Some(MenuItemEmpty("units"))
  override def WSW = Some(MenuItem('Polyline, TextShape("Feet", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def SSW = Some(MenuItem('Polyline, RadialMenuIcon.sampleProperties))
  override def S   = Some(MenuItemEmpty("sampling"))
}
