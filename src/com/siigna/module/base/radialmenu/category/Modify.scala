/*
 * Copyright (c) 2012. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.base.radialmenu.category

import com.siigna.module.base.radialmenu._

case class Modify(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.modifyColor

  def name = "Modify"

  override def NNE = Some(MenuItem('Polyline, RadialMenuIcon.align))
  override def ENE = Some(MenuItem('Polyline, RadialMenuIcon.fillet))
  override def ESE = Some(MenuItem('Polyline, RadialMenuIcon.chamfer))
  override def NNW = Some(MenuItem('Scale, RadialMenuIcon.scale, "modify"))
  override def SSW = Some(MenuItem('Trim, RadialMenuIcon.trim, "modify"))
  override def SSE = Some(MenuItem('Polyline, RadialMenuIcon.extend))
  
}
