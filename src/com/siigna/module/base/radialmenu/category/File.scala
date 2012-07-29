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

import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D
import com.siigna.app.model.shape.TextShape

case class File(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.fileColor

  def name = "File"

  override def E = Some(MenuItemEmpty("local"))
  override def N = Some(MenuItemEmpty("settings"))
  //override def W = Some(MenuItemEmpty("server"))
  override def S = Some(MenuItemEmpty("print"))

  override def NNE = Some(MenuItem('SetTitle, TextShape("title", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "file"))
  override def NNW = Some(MenuItem('SetTitle, TextShape("openness", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "file"))

  override def SSE = Some(MenuItem('Print, RadialMenuIcon.print, "file"))
  //override def SSW = Some(MenuItem('ClearDatabase, RadialMenuIcon.clearDB, "file"))
  override def ESE = Some(MenuItem('Export, RadialMenuIcon.exportDXF, "com.siigna.module.io"))
  override def ENE = Some(MenuItem('Import, RadialMenuIcon.importDXF, "com.siigna.module.io"))
  //override def WSW = Some(MenuItem('Open, RadialMenuIcon.load, "file"))
  //override def WNW = Some(MenuItem('Save, RadialMenuIcon.save, "file"))

}