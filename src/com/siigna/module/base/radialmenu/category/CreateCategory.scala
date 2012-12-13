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
import com.siigna._

case object CreateCategory extends MenuCategory{
  val graph = Map[MenuEvent,MenuElement](
    EventN -> GeometryCategory,

    EventNNW -> MenuModule(Module('Rectangle,"com.siigna.module.base.create"), MenuIcons.rectangle),
    EventNNE -> MenuModule(Module('Line,"com.siigna.module.base.create"), MenuIcons.line),

    EventENE -> MenuModule(Module('Offset,"com.siigna.module.base.create"), MenuIcons.offset),

    EventWNW -> MenuModule(Module('Text,"com.siigna.module.base.create"), MenuIcons.text),
    EventSSE -> MenuModule(Module('Explode,"com.siigna.module.base.create"), MenuIcons.explode),
    EventWSW -> MenuModule(Module('Lineardim,"com.siigna.module.base.create"), MenuIcons.linearDimension),
    EventSSW -> MenuModule(Module('Fill,"com.siigna.module.base.create"), MenuIcons.raster)

  )

  val color = MenuIcons.createColor

  val parent = Some(StartCategory)
}
