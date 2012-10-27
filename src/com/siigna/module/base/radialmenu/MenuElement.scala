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

package com.siigna.module.base.radialmenu

import com.siigna.app.model.shape.Shape
import com.siigna.module.ModuleInstance

trait MenuElement {
  def icon : Iterable[Shape]
}

/**
 * A MenuItem is an item in the [[com.siigna.module.base.Menu]].
 *
 */
trait MenuCategory extends MenuElement {
  /**
   * The color used for background-filling.
   */
  def color : java.awt.Color

  val icon = RadialMenuIcon.C

  def graph: Map[MenuEvent, MenuElement]

  /**
   * The parent of the category if any.
   */
  def parent : Option[MenuCategory]
}

/**
 * A MenuItem is an item in the [[com.siigna.module.base.Menu]].
 */
case class MenuModule(instance: ModuleInstance, icon: Iterable[Shape]) extends MenuElement
