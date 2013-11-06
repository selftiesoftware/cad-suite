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

package com.siigna.module.base.radialmenu.category

import com.siigna.module.base.radialmenu._

import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D
import com.siigna.app.model.shape.TextShape
import com.siigna.module.base.radialmenu.MenuModule
import com.siigna._
import module.cad.radialmenu.category.StartCategory

case object FileCategory extends MenuCategory{

  val graph = Map[MenuEvent,MenuElement](

    EventWNW -> MenuModule(Module('porter,"Export"), Iterable(TextShape("export", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))))),
    EventWSW -> MenuModule(Module('porter,"Import"), Iterable(TextShape("import", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))

    //EventESE -> MenuModule(Module('Export,"com.siigna.module.io"), MenuIcons.exportDXF),
    //EventENE -> MenuModule(Module('Import,"com.siigna.module.io"), MenuIcons.importDXF)
  )

  val color = MenuIcons.fileColor

  val parent = Some(StartCategory)
}