/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.base.radialmenu.category

import com.siigna.app.model.shape.TextShape
import com.siigna.module.base.radialmenu._
import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D

case class Properties(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.propertiesColor

  def name = "Properties"

  override def E   = Some(MenuItemEmpty("util"))
  override def ESE = Some(MenuItem('Performancetest, TextShape("test", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def ENE = Some(MenuItem('AntiAliasingToggle, TextShape("Anti-Alias", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def N   = Some(MenuItemEmpty("style"))
  override def NNE = Some(MenuItem('Polyline, RadialMenuIcon.NNEArrow))
  override def NNW = Some(MenuItem('Polyline, RadialMenuIcon.NNWArrow))
  override def WNW = Some(MenuItem('Polyline, TextShape("mm", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def W   = Some(MenuItemEmpty("units"))
  override def WSW = Some(MenuItem('Polyline, TextShape("Feet", Vector2D(0, 0), 8, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def SSW = Some(MenuItem('Polyline, RadialMenuIcon.sampleProperties))
  override def S   = Some(MenuItemEmpty("sampling"))
}
