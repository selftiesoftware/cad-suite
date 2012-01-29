/* 2010 (C) Copyright by Siigna, all rights reserved. */

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
  override def WNW = Some(MenuItem('Weight, RadialMenuIcon.weight))
  //override def WNW = Some(new MenuItem('Polyline, RadialMenuIcon.sampleProperties))
  override def WSW = Some(MenuItem('ColorWheel, RadialMenuIcon.colorWheel))
  override def SSW = Some(MenuItem('Rotate, RadialMenuIcon.rotate))
  override def SSE = Some(MenuItem('Move, RadialMenuIcon.move, "modify"))
  override def ESE = Some(MenuItem('Polyline, RadialMenuIcon.guides))

}
