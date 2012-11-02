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
import com.siigna.module.base.radialmenu.MenuModule
import com.siigna._

case object FileCategory extends MenuCategory{


  val graph = Map[MenuEvent,MenuElement](

    EventNNE -> MenuModule(Module('SetTitle,"com.siigna.module.base.file"), Iterable(TextShape("title", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))))),

    EventSSE -> MenuModule(Module('Print,"com.siigna.module.base.file"), RadialMenuIcon.print),
    EventESE -> MenuModule(Module('Export,"com.siigna.module.io"), RadialMenuIcon.exportDXF),
    EventENE -> MenuModule(Module('Import,"com.siigna.module.io"), RadialMenuIcon.importDXF)
  )

  val color = RadialMenuIcon.fileColor

  val parent = Some(StartCategory)
}