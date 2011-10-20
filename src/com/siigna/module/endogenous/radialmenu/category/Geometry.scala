/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._
import com.siigna.module.endogenous.radialmenu.MenuItemEmpty._

case class Geometry(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.createColor

  def name = "Geometry"


override def N = Some(MenuItemEmpty("Freehand"))

    override def NNE = Some(new MenuItem('Artline, RadialMenuIcon.circle))

}
