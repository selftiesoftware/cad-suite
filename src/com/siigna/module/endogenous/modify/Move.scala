package com.siigna.module.endogenous.modifyCategory

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