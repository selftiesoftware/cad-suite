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

/*package com.siigna.module.cad.file

import com.siigna._
import module.base.ModuleInit

class SetTitle extends Module {

  var boundary = Drawing.boundary

  var text         = ""

  def stateMap = Map(

    'StartCategory -> {
      //TODO: A CLICK IS NEEDED BEFORE SIIGNA REGISTERES KEY INPUT. TO BE FIXED.
      View.zoom(ModuleInit.titleFocus.get, -2)
      Siigna display "type a drawing title"
      'TextInput
    },
    'TextInput -> {
      case events => {
        events match {
          case KeyDown(Key.Backspace, _) :: tail => {
            if (text.length != 0) {
              text = text.substring(0, text.length - 1)
              Siigna display text
            }
            else 'End
          }
          case KeyDown(Key.Enter, _) :: tail => 'End
          case KeyDown(Key.Esc, _) :: tail => {
            text = ""
            'End
          }
          case KeyDown(key, _) :: tail => {
            text += key.toChar.toString.toLowerCase        }
          Siigna display text

          case MouseUp(_, MouseButtonRight, _) :: tail => 'End
          case _ =>
        }
        None
      }
    },
    'End -> {
      //save a new name in the databasen (automatically corrects the applet's variable)
      Set(DrawingName, text)

      //reset the vars
      text = ""
    }
  )
}*/