/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.app.model.shape.TextShape
import com.siigna.module.endogenous.radialmenu._
import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D

case class Properties(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.propertiesColor

  def name = "Properties"

  override def NNE = Some(MenuItem('AntiAliasingToggle, TextShape("Anti-Alias", Vector2D(0, 0), 10, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def N   = Some(MenuItemEmpty("Weight"))
  override def NNW = Some(MenuItem('Polyline, RadialMenuIcon.NNWArrow))
  override def WNW = Some(MenuItem('Polyline, TextShape("mm", Vector2D(0, 0), 10, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def WSW = Some(MenuItem('Polyline, TextShape("Feet", Vector2D(0, 0), 10, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
  override def ESE = Some(MenuItem('Polyline, RadialMenuIcon.styles))

}
