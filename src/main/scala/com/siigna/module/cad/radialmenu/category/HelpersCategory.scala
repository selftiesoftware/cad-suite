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

import com.siigna.module.base.radialmenu._
import com.siigna._

case object HelpersCategory extends MenuCategory {
  val graph = Map[MenuEvent,MenuElement](
    EventENE -> MenuModule(Module('cad,"helpers.Snap"), MenuIcons.snap),
    EventESE -> MenuModule(Module('cad,"helpers.Track"), MenuIcons.guides),


      //EventWNW -> MenuModule(Module('cad,"helpers.Grid"), MenuIcons.grid),

    EventSSW -> MenuModule(Module('cad,"helpers.Area"), MenuIcons.area),
    EventNNE -> MenuModule(Module('cad,"helpers.Distance"), MenuIcons.distance)

  )

  val color = MenuIcons.helpersColor
  val parent = Some(StartCategory)
}