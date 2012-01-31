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

/* 2010 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._

object Move extends Module {

  var startPoint : Option[Vector2D] = None
  var doneDrawing = false
  var closeToObjectOnStart = false

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start      -> 'KeyEscape -> 'End,
    'Start      -> 'Message   -> 'FirstPoint,
    'FirstPoint -> 'Message   -> 'End)
  
  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      // TODO: Make sure objects are selected
      Siigna.display("Select a starting point to move from.")

      ForwardTo('Point)
    }),
    'FirstPoint -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          startPoint = Some(p)
          ForwardTo('Point)
        }
        case _ => ForwardTo('Start)
      }
    }),
    'End   -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail if (startPoint.isDefined) => {
          Log(p - startPoint.get)
        }
        case _ => {
          Goto('FirstPoint)
        }
      }
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {

  }

}