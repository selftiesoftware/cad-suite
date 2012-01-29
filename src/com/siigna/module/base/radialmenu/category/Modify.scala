/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.base.radialmenu.category

import com.siigna.module.base.radialmenu._

case class Modify(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.modifyColor

  def name = "Modify"

  override def NNE = Some(MenuItem('Polyline, RadialMenuIcon.align))
  override def ENE = Some(MenuItem('Polyline, RadialMenuIcon.fillet))
  override def ESE = Some(MenuItem('Polyline, RadialMenuIcon.chamfer))
  override def NNW = Some(MenuItem('Polyline, RadialMenuIcon.scale))
  override def SSW = Some(MenuItem('Trim, RadialMenuIcon.trim))
  override def SSE = Some(MenuItem('Polyline, RadialMenuIcon.extend))
  
}
