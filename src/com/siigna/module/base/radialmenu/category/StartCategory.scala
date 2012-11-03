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

import com.siigna._
import module.base.radialmenu._
import module.base.radialmenu.MenuCategory
import module.base.radialmenu.MenuModule
import module.base.radialmenu.MenuIcons


case object StartCategory extends MenuCategory{
  val graph = Map[MenuEvent,MenuElement](
    //main cetegories in the start menu:
    EventC -> FileCategory,
    EventE -> HelpersCategory,
    EventN -> CreateCategory,
    EventW -> PropertiesCategory,
    EventS -> ModifyCategory,
    //drawing tools in the default menu:
    EventENE -> MenuModule(Module('SnapToggle,"com.siigna.module.base.helpers"), MenuIcons.snap),
    EventNNE -> MenuModule(Module('Polyline,"com.siigna.module.base.create"), MenuIcons.polyline),
    EventNNW -> MenuModule(Module('Copy,"com.siigna.module.base.create"), MenuIcons.copy),
    EventWNW -> MenuModule(Module('Stroke,"com.siigna.module.base.properties"), MenuIcons.weight),
    EventWSW -> MenuModule(Module('Colors,"com.siigna.module.base.properties"), MenuIcons.colorWheel),
    EventSSW -> MenuModule(Module('Rotate,"com.siigna.module.base.modify"), MenuIcons.rotate),
    EventSSE -> MenuModule(Module('Move,"com.siigna.module.base.modify"), MenuIcons.move),
    EventESE -> MenuModule(Module('TrackToggle,"com.siigna.module.base.helpers"), MenuIcons.guides)
  )

  val color = MenuIcons.fileColor
  val parent = None
}
