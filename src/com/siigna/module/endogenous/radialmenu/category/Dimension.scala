/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu.RadialMenuIcon

case class Dimension(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.createColor
  def name = "Dim.line"

}
