/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._

case class Helpers(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.helpersColor

  def name = "Helpers"

  override def ENE = Some(MenuItem('Polyline, RadialMenuIcon.endPointSnap))
  override def NNE = Some(MenuItem('Show, RadialMenuIcon.show))
  override def NNW = Some(MenuItem('Hide, RadialMenuIcon.hide))
  override def WNW = Some(MenuItem('Polyline, RadialMenuIcon.selectSame))
  override def WSW = Some(MenuItem('Polyline, RadialMenuIcon.selectAll))
  override def SSW = Some(MenuItem('Area, RadialMenuIcon.area))
  override def SSE = Some(MenuItem('Distance, RadialMenuIcon.distance))
  override def ESE = Some(MenuItem('Polyline, RadialMenuIcon.midPointSnap))

}
