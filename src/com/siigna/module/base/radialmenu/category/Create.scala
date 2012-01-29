/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.base.radialmenu.category

import com.siigna.module.base.radialmenu._

case class Create(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.createColor

  def name = "Create"

  override def E = Some(MenuItemEmpty("Create from"))
  override def N = Some(MenuItemEmpty("Geometry"))
  override def W = Some(MenuItemEmpty("Annotation"))
  override def S = Some(MenuItemEmpty("Collection"))
  //override def S = Some(new Dimension(Some(this)))

  override def ENE = Some(MenuItem('Copy, RadialMenuIcon.copy))
  override def NNE = Some(MenuItem('Rectangle, RadialMenuIcon.rectangle))
  //override def NNW = Some(new MenuItem('Circle, RadialMenuIcon.circle))
  override def WNW = Some(MenuItem('Text, RadialMenuIcon.text))
  override def NNW = Some(MenuItem('Raster, RadialMenuIcon.raster))
  override def SSW = Some(MenuItem('Polyline,RadialMenuIcon.group))
  override def SSE = Some(MenuItem('Polyline, RadialMenuIcon.explode))
  override def WSW = Some(MenuItem('Lineardim, RadialMenuIcon.linearDimension))
  override def ESE = Some(MenuItem('Polyline, RadialMenuIcon.offset))

}
