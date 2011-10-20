/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._

case class Create(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.createColor

  def name = "Create"

  override def E = Some(new CreateFrom(Some(this)))
  override def N = Some(new Geometry(Some(this)))
  override def W = Some(MenuItemEmpty("Group"))
  override def S = Some(new Dimension(Some(this)))

  override def ENE = Some(new MenuItem('Copy, RadialMenuIcon.copy))
  override def NNE = Some(new MenuItem('Circle, RadialMenuIcon.circle))
  override def NNW = Some(new MenuItem('Arc, RadialMenuIcon.arc))
  override def WNW = Some(new MenuItem('Polyline, RadialMenuIcon.group))
  override def WSW = Some(new MenuItem('Polyline, RadialMenuIcon.explode))
  override def SSW = Some(new MenuItem('Polyline, RadialMenuIcon.angularDimension))
  override def SSE = Some(new MenuItem('Lineardim, RadialMenuIcon.linearDimension))
  override def ESE = Some(new MenuItem('Polyline, RadialMenuIcon.offset))

}
