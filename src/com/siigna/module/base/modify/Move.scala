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

object Move extends Module {

  var endPoint : Option[Vector2D] = None

  var startPoint : Option[Vector2D] = None
  
  var transformation : Option[TransformationMatrix] = None

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start -> 'KeyDown -> 'End,
    'Start -> 'MouseUp -> 'End,
    'Move  -> 'MouseUp -> 'End,
    'Move  -> 'KeyDown -> 'End
  )
  
  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case Message(p : Option[Vector2D]) :: tail => startPoint = p
        case MouseDown(p, _, _) :: tail => startPoint = Some(p)
        case MouseMove(p, _, _) :: tail => startPoint = Some(p)
        case MouseDrag(p, _, _) :: tail => startPoint = Some(p)
        case _ =>
      }

      if (Model.selection.isDefined && startPoint.isDefined) {
        Goto('Move)
      } else {
        Goto('End)
      }
    }),
    'Move -> ((events : List[Event]) => {
      if (startPoint.isDefined) {
        val translation = events match {
          case MouseDown(p, _, _) :: tail => {
            endPoint = Some(p)
            p - startPoint.get
          }
          case MouseDrag(p, _, _) :: tail => {
            endPoint = Some(p)
            p - startPoint.get
          }
          case MouseMove(p, _, _) :: tail => {
            endPoint = Some(p)
            p - startPoint.get
          }
          case _ => Vector2D(0, 0)
        }

        transformation = Some(TransformationMatrix(translation, 1))
        Model.selection.get.transform(transformation.get)
      }
    }),
    'End   -> ((events : List[Event]) => {
      //deselect, but only if an objects has been moved.
      if (Model.selection.isDefined && (startPoint.get - endPoint.get != Vector2D(0, 0))) {
        Model.deselect()
      }
    })
  )
}