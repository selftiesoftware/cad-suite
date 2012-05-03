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

case class Helpers(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.helpersColor

  def name = "Helpers"

  override def N = Some(MenuItemEmpty("Objects"))
  override def E = Some(MenuItemEmpty("Toggle snap"))
  override def S = Some(MenuItemEmpty("Measure"))
  override def W = Some(MenuItemEmpty("Guidelines"))

  override def ENE = Some(MenuItem('SnapToggle, RadialMenuIcon.endPointSnap, "helpers"))
  override def NNE = Some(MenuItem('Hide, RadialMenuIcon.hide))
  override def NNW = Some(MenuItem('Polyline, RadialMenuIcon.selectSame))
  override def WNW = Some(MenuItem('Polyline, RadialMenuIcon.grid))
  override def WSW = Some(MenuItem('Polyline, RadialMenuIcon.perspective))
  override def SSW = Some(MenuItem('Area, RadialMenuIcon.area))
  override def SSE = Some(MenuItem('Distance, RadialMenuIcon.distance, "helpers"))
  override def ESE = Some(MenuItem('Polyline, RadialMenuIcon.midPointSnap))

}
