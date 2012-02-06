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

case object Start extends MenuCategory {

  val color = RadialMenuIcon.fileColor

  val parent = None
  
  def name = "Start"

  override def C = Some(new File(Some(this)))
  override def E = Some(new Helpers(Some(this)))
  override def N = Some(new Create(Some(this)))
  override def W = Some(new Properties(Some(this)))
  override def S = Some(new Modify(Some(this)))

  override def ENE = Some(MenuItem('Polyline, RadialMenuIcon.snap))
  override def NNE = Some(MenuItem('Polyline, RadialMenuIcon.polyline))
  override def NNW = Some(MenuItem('Artline, RadialMenuIcon.artline))
  override def WNW = Some(MenuItem('Weight, RadialMenuIcon.weight, "properties"))
  //override def WNW = Some(new MenuItem('Polyline, RadialMenuIcon.sampleProperties))
  override def WSW = Some(MenuItem('ColorWheel, RadialMenuIcon.colorWheel, "properties"))
  override def SSW = Some(MenuItem('Rotate, RadialMenuIcon.rotate, "modify"))
  override def SSE = Some(MenuItem('Move, RadialMenuIcon.move, "modify"))
  override def ESE = Some(MenuItem('Polyline, RadialMenuIcon.guides))

}
