/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._

case class Helpers(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.helpersColor

  def name = "Helpers"

  override def ENE = Some(new MenuItem('Polyline, RadialMenuIcon.endPointSnap))
  override def NNE = Some(new MenuItem('Show, RadialMenuIcon.show))
  override def NNW = Some(new MenuItem('Hide, RadialMenuIcon.hide))
  override def WNW = Some(new MenuItem('Polyline, RadialMenuIcon.selectSame))
  override def WSW = Some(new MenuItem('Performancetest, RadialMenuIcon.selectAll))
  override def SSW = Some(new MenuItem('Polyline, RadialMenuIcon.area))
  override def SSE = Some(new MenuItem('Distance , RadialMenuIcon.distance))
  override def ESE = Some(new MenuItem('Polyline, RadialMenuIcon.midPointSnap))

}
