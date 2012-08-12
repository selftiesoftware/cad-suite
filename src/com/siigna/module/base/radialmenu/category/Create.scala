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

case class Create(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.createColor

  def name = "Create"

  //override def E = Some(MenuItemEmpty("Create from"))
  override def N = Some(new Geometry(Some(this)))
  override def W = Some(CategoryItemEmpty("Annotation"))
  override def S = Some(CategoryItemEmpty("Collection"))
  //override def S = Some(new Dimension(Some(this)))
  override def E = Some(CategoryItemEmpty("Script"))

  override def NNW = Some(MenuItem('Rectangle, RadialMenuIcon.rectangle))
  override def NNE = Some(MenuItem('Line, RadialMenuIcon.line , "create"))

  override def WNW = Some(MenuItem('Text, RadialMenuIcon.text))
  //override def NNW = Some(MenuItem('Fill, RadialMenuIcon.raster, "create"))
  //override def SSW = Some(MenuItem('Polyline, RadialMenuIcon.group))
  override def SSE = Some(MenuItem('Explode, RadialMenuIcon.explode, "create"))
  override def WSW = Some(MenuItem('Lineardim, RadialMenuIcon.linearDimension))
  //override def ESE = Some(MenuItem('Offset, RadialMenuIcon.offset, "create"))

  override def ESE = Some(MenuItem('ScriptEditor, TextShape("editor", Vector2D(0, 0), 7, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "create"))
  override def ENE = Some(MenuItem('ScriptExecution, TextShape("run", Vector2D(0, 0), 7, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))), "create"))

}
