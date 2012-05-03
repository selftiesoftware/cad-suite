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

package com.siigna.module.base.create

import com.siigna._

object Offset extends Module {

  var distance : Option[Vector2D] = None

  val eventHandler = EventHandler(stateMap, stateMachine)

  var originals : Option[Selection] = None

  val shapeGuide : Vector2D => Traversable[Shape] = (v : Vector2D) => {
    // Create a matrix
    val t : TransformationMatrix = TransformationMatrix(v, 1)
    Model.selection.get.apply(t)
  }

  var text  = ""

  def stateMap = DirectedGraph(
    'Start         -> 'KeyEscape ->         'End
  )

  //Select shapes
  def stateMachine = Map(
  'Start -> ((events : List[Event]) => {
    if (!Model.selection.isDefined) {
      Siigna display "No objects selected"
      Goto('End)
    }
    else {
      events match {

        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ => {
          originals = Some(Model.selection.get)
          Goto('SetDistance)
        }
      }
    }
  None
  }),
  //Forward to Point to get offset point
  'SetDistance -> ((events : List[Event]) => {
    events match {
      case KeyDown(_ ,_ ) :: tail => Goto('TextInput)
      case Message(p : Vector2D) :: tail => {
        distance = Some(p)
        Goto('End)
      }
      case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
      case MouseUp(p, _, _) :: tail => {
        //goto 'Point to get the offset distance
        ForwardTo('Point, false)
        Controller ! Message(PointGuides(shapeGuide))
      }
      case _ =>
    }
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
          text += key.toChar.toString.toLowerCase
          Siigna display text
        }
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ =>
      }
      None
    }),
    //do the offset calculation here
    'End -> ((events : List[Event]) => {
    println("offset here")
  }))
}