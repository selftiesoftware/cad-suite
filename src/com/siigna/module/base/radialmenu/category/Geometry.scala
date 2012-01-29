/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.base.radialmenu.category

import com.siigna.module.base.radialmenu._

case class Geometry(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.createColor

  def name = "Geometry"

  override def NNW = Some(MenuItem('Circle, RadialMenuIcon.circle))

  override def N = Some(MenuItemEmpty("Freehand"))

    override def NNE = Some(MenuItem('Artline, RadialMenuIcon.circle))

}
