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

package com.siigna.module.base.file

import com.siigna._
import com.siigna.app.model.drawing.activeDrawing._

object SetTitle extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  var text         = ""

  def stateMap = DirectedGraph (
    'Start    -> 'Event     -> 'End
  )

  def stateMachine = Map(

    'Start -> ((events : List[Event]) => {
      Goto('TextInput)
      None
    }),
    'TextInput -> ((events : List[Event]) => {
      events match {
        case KeyDown(Key.Backspace, _) :: tail => {
            if (text.length != 0) text = text.substring(0, text.length - 1)
            else Goto('End)
        }
        case KeyDown(Key.Enter, _) :: tail => Goto('End)
        case KeyDown(Key.Esc, _) :: tail => {
          text = ""
          Goto('End)
        }
        case KeyDown(key, _) :: tail => {
          text += key.toChar.toString.toLowerCase        }
          println(text)

        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ =>
      }
      None
    }),
    'End -> ((events : List[Event]) => {
      //set drawing title here
      //println("set title here")
      None
      println(text)
      //set the drawing title
      setActiveDrawingName(5,text)

      //reset the vars
      text = ""
    })
  )
}