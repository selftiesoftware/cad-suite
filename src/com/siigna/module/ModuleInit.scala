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

package com.siigna.module

import base.Menu
import base.radialmenu.category.StartCategory
import com.siigna._

/**
 * An init module for the cad-suite.
 */
class ModuleInit extends Module {

  Menu.startCategory = StartCategory

  protected var lastModule : Option[ModuleInstance] = None

  def stateMap = Map(
    'Start -> {
      // Match for modules to forward to
      case End(module : ModuleInstance) :: tail => {
        lastModule = Some(module) // Store it as a last module
        Start(module) // Forward
      }
      case MouseDown(_, MouseButtonRight, _) :: tail => Start('Menu, "com.siigna.module.base")
      case MouseDown(_, _, _) :: tail                => Start('Selection, "com.siigna.module.base")

      case KeyDown('c', _) :: KeyUp('p', _) :: tail => {
        Start('Colors, "com.siigna.module.base.properties")
      }
      case KeyDown('l', _) :: KeyUp('c', _) :: tail => {
        Start('Line, "com.siigna.module.base.create")
      }
      case KeyDown('p', _) :: KeyUp('c', _) :: tail => {
        Start('Polyline, "com.siigna.module.base.create")
      }
      case KeyDown('c', _) :: KeyUp('c', _) :: tail => {
        Start('Circle, "com.siigna.module.base.create")
      }

      case KeyDown('a', Control) :: tail => Drawing.selectAll()
      case KeyDown('z', Control) :: tail => Drawing.undo()
      case KeyDown('y', Control) :: tail => Drawing.redo()

      // Forward to the last initiated module
      case KeyDown(Key.Space, _) :: tail => if (lastModule.isDefined) Start(lastModule.get.copy)

      // Release all selections
      case KeyDown(Key.Esc, _) :: tail => Drawing.deselect()
      case _ =>
    }
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    //println("draw guides here!!")
  }
}
