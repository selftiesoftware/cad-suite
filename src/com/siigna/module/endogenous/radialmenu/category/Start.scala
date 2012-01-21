/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._

case object Start extends MenuCategory {

  val color = RadialMenuIcon.fileColor

  val parent = None
  
  def name = "Start"

  override def C = Some(new File(Some(this)))
  override def E = Some(new Helpers(Some(this)))
  override def N = Some(new Create(Some(this)))
  override def W = Some(new Properties(Some(this)))
  override def S = Some(new Modify(Some(this)))

  override def ENE = Some(new MenuItem('Polyline, RadialMenuIcon.snap))
  override def NNE = Some(new MenuItem('Polyline, RadialMenuIcon.polyline))
  override def NNW = Some(new MenuItem('Artline, RadialMenuIcon.artline))
  override def WNW = None
  //override def WNW = Some(new MenuItem('Polyline, RadialMenuIcon.sampleProperties))
  override def WSW = Some(new MenuItem('ColorWheel, RadialMenuIcon.colorWheel))
  override def SSW = Some(new MenuItem('Raster, RadialMenuIcon.rotate))
  override def SSE = Some(new MenuItem('Move, RadialMenuIcon.move))
  override def ESE = Some(new MenuItem('Polyline, RadialMenuIcon.guides))

}
