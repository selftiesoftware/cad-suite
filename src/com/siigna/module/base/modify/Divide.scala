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

package com.siigna.module.base.modify

import com.siigna._
import com.siigna.module.base.Menu._

object Divide extends Module {

/**
 * A module to split a line into a number (specified by the user) of segments. Each segment has the length lineLength/segments.
 */
  var endPoint : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None
  //var transformation : Option[TransformationMatrix] = None

  def eventHandler = EventHandler(stateMap, stateMachine)

  val transformation = TransformationMatrix()
  var text         = ""

  def stateMap     = DirectedGraph(
    'Start -> 'KeyDown -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //start 'Move only if there is a selection

       startPoint = Some(Vector2D(0,0))
       endPoint = Some(Vector2D(100,100))

      if (!Model.selection.isEmpty) {
        Siigna display "type segments"
        Goto('TextInput)
      }
      // if no selection is made, go to the selection module
      else {
        Siigna display "Select line to divide"
        ForwardTo('Selection)
      }
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
          Siigna display text

        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ =>
      }
      None
    }),
    'End   -> ((events : List[Event]) => {


       //make the division here:
    })
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    if(startPoint.isDefined && endPoint.isDefined){
      println("painting")
      g draw LineShape(startPoint.get,endPoint.get).transform(t)
    }
  }
}