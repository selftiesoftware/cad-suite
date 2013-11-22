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
import com.siigna.TextShape
import com.siigna.Vector2D
import com.siigna.Attributes

case object HelpersCategory extends MenuCategory {
  val graph = Map[MenuEvent,MenuElement](
    //N
    EventNNE -> MenuModule(Module('cad,"helpers.SnapToggle"), MenuIcons.snap),
    EventNNW -> MenuModule(Module('cad,"helpers.TrackToggle"), MenuIcons.guides),
    //E
    EventENE -> MenuModule(Module('cad,"helpers.Distance"), MenuIcons.distance),
    EventESE -> MenuModule(Module('cad,"helpers.ZoomExtends"), MenuIcons.zoomExtends),
    //W
    //EventWSW -> MenuModule(Module('cad,"helpers.ImageBackground"), Iterable(TextShape("image", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))))),
    //EventWNW -> MenuModule(Module('cad,"helpers.Grid"), MenuIcons.grid),
    //SS
    EventSSE -> MenuModule(Module('cad,"helpers.Area"), MenuIcons.area),
    EventSSW -> MenuModule(Module('cad,"helpers.TooltipToggle"), Iterable(TextShape("tooltips", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
)

  val color = MenuIcons.helpersColor
  val parent = Some(StartCategory)
}