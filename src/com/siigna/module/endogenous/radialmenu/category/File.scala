/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._

import com.siigna.util.collection.Attributes
import com.siigna.util.geom.Vector2D
import com.siigna.app.model.shape.TextShape

case class File(parent : Option[MenuCategory]) extends MenuCategory {

  val color = RadialMenuIcon.fileColor

    def name = "File"

    override def NNE = Some(MenuItem('Export, TextShape("Embed", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
    override def NNW = Some(MenuItem('Export, TextShape("Get URL", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
    override def SSE = Some(MenuItem('Print, RadialMenuIcon.print))
    override def ESE = Some(MenuItem('Export, TextShape("Export", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
    override def ENE = Some(MenuItem('Import, TextShape("Import", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
    override def WSW = Some(MenuItem('Open, TextShape("Open", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))
    override def WNW = Some(MenuItem('Save, TextShape("Save", Vector2D(0, 0), 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5)))))

}