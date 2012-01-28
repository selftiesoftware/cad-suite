/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._

case class Create(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.createColor

  def name = "Create"

  override def E = Some(MenuItemEmpty("Create from"))
  override def N = Some(MenuItemEmpty("Geometry"))
  override def W = Some(MenuItemEmpty("Annotation"))
  override def S = Some(MenuItemEmpty("Collection"))
  //override def S = Some(new Dimension(Some(this)))

  override def ENE = Some(new MenuItem('Copy, RadialMenuIcon.copy))
  override def NNE = Some(new MenuItem('Rectangle, RadialMenuIcon.rectangle))
  //override def NNW = Some(new MenuItem('Circle, RadialMenuIcon.circle))
  override def WNW = Some(new MenuItem('Text, RadialMenuIcon.text))
  override def NNW = Some(new MenuItem('Raster, RadialMenuIcon.raster))
  override def SSW = Some(new MenuItem('Polyline,RadialMenuIcon.group))
  override def SSE = Some(new MenuItem('Polyline, RadialMenuIcon.explode))
  override def WSW = Some(new MenuItem('Lineardim, RadialMenuIcon.linearDimension))
  override def ESE = Some(new MenuItem('Polyline, RadialMenuIcon.offset))

}
