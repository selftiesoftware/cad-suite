/*
 * Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.cad.radialmenu.category

import com.siigna.module.base.radialmenu._
import com.siigna._

case object ModifyCategory extends MenuCategory{
  val graph = Map[MenuEvent,MenuElement](
    EventSSE -> MenuModule(Module('cad,"modify.Move"), MenuIcons.move),
    EventSSW -> MenuModule(Module('cad,"modify.Rotate"), MenuIcons.rotate),
    EventWSW -> MenuModule(Module('cad,"modify.Scale"), MenuIcons.scale)
    //EventNNE -> MenuModule(Module('cad,"modify.Trim"), MenuIcons.trim)
    //EventENE -> MenuModule(Module('cad,"modify.Connect"), MenuIcons.connect)

  )

  val color = MenuIcons.modifyColor

  val parent = Some(StartCategory)
}
