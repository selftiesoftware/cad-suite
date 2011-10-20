/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._

case class Modify(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.modifyColor

  def name = "Modify"

  override def NNE = Some(new MenuItem('Polyline, RadialMenuIcon.align))
  override def ENE = Some(new MenuItem('Polyline, RadialMenuIcon.fillet))
  override def ESE = Some(new MenuItem('Polyline, RadialMenuIcon.chamfer))
  override def NNW = Some(new MenuItem('Polyline, RadialMenuIcon.scale))
  override def SSW = Some(new MenuItem('Trim, RadialMenuIcon.trim))
  override def SSE = Some(new MenuItem('Polyline, RadialMenuIcon.extend))
  
}
