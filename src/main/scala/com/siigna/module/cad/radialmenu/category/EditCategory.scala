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

case object EditCategory extends MenuCategory{
  val graph = Map[MenuEvent,MenuElement](

    //Translations
    EventNNW -> MenuModule(Module('cad,"edit.Rotate"), MenuIcons.rotate),
    EventNNE -> MenuModule(Module('cad,"edit.Scale"), MenuIcons.scale),
    //EventENE -> MenuModule(Module('cad,"edit.Mirror"), MenuIcons.mirror)

    //Intersections
    EventENE -> MenuModule(Module('cad,"edit.Trim"), MenuIcons.trim),

    //Properties
    EventWNW -> MenuModule(Module('cad,"edit.Stroke"), MenuIcons.stroke),
    EventWSW -> MenuModule(Module('cad,"edit.Colors"), MenuIcons.colorWheel),

    //Segments
    EventSSW -> MenuModule(Module('cad,"edit.Explode"), MenuIcons.explode),
    EventSSE -> MenuModule(Module('cad,"edit.Join"), MenuIcons.connect)

  )
  val color = MenuIcons.editColor
  val parent = Some(StartCategory)
}