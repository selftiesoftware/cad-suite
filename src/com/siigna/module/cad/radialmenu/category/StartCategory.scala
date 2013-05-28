/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.radialmenu.category

import com.siigna._
import module.base.Menu
import module.base.radialmenu._
import module.base.radialmenu.category.FileCategory
import module.base.radialmenu.MenuCategory
import module.base.radialmenu.MenuModule
import module.base.radialmenu.MenuIcons


case object StartCategory extends MenuCategory{

  Menu.startCategory = this

  val graph = Map[MenuEvent,MenuElement](
    //main cetegories in the start menu:
    EventC -> FileCategory,
    EventE -> HelpersCategory,
    EventN -> CreateCategory,
    EventW -> PropertiesCategory,
    EventS -> ModifyCategory,

    //drawing tools in the default menu:
    //EventENE -> MenuModule(Module('SnapToggle,"com.siigna.module.base.helpers"), MenuIcons.snap),
    EventNNE -> MenuModule(Module('cad,"create.Polyline"), MenuIcons.polyline),
    EventNNW -> MenuModule(Module('cad,"create.Copy"), MenuIcons.copy),
    EventWNW -> MenuModule(Module('cad,"properties.Stroke"), MenuIcons.weight),
    EventWSW -> MenuModule(Module('cad,"properties.Colors"), MenuIcons.colorWheel),
    EventSSW -> MenuModule(Module('cad,"modify.Rotate"), MenuIcons.rotate),
    EventSSE -> MenuModule(Module('cad,"modify.Move"), MenuIcons.move),
    EventESE -> MenuModule(Module('cad,"helpers.TrackToggle"), MenuIcons.guides),
    EventENE -> MenuModule(Module('cad,"helpers.SnapToggle"), MenuIcons.snap)
  )

  val color = MenuIcons.fileColor
  val parent = None
}
