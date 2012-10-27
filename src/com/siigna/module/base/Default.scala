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

package com.siigna.module.base
import com.siigna._
import java.awt.Color

/**
* The default module for the base module pack. Works as access point to
* the rest of the modules.
 */
object Default extends Module {

  def stateMap = Map(
    'Start -> {
      case e => {
        println(e)
        e match {
          case MouseDown(_, MouseButtonRight, _) :: tail => {
            println(e.head)
            Module('Menu)
          }
          case KeyDown('l', _) :: tail => {
            Create(LineShape(Vector2D(0, 0), Vector2D(100, 100)))
          }
          case KeyDown('c', _) :: tail => {
            Create(CircleShape(Vector2D(100, 100), 12))
          }
          case KeyDown('z', Control) :: tail => {
            Drawing.undo()
          }
          case KeyDown('y', Control) :: tail => {
            Drawing.redo()
          }
          case _ =>
        }
      }
    }
  )

  override def paint(g : Graphics, t : TransformationMatrix) {

  }
}