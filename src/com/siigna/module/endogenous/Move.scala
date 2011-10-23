/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna.module.Module

import com.siigna._

object Move extends Module {

  var basePoint : Option[Vector] = None
  var endPoint  : Option[Vector] = None
  var doneDrawing = false
  var closeToObjectOnStart = false

  def delta : Option[Vector] = if (endPoint.isDefined && basePoint.isDefined) Some(endPoint.get - basePoint.get) else None

  var shapes : Seq[Shape] = Seq()

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End,
                                        'Start     -> 'MouseDrag -> 'Move,
                                        'Start     -> 'MouseUp   -> 'End,
                                        'Move      -> 'KeyEscape -> 'End,
                                        'Move      -> 'MouseUp   -> 'End)
  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case KeyDown(Key.Control, _) :: tail => Goto('End); ForwardTo('Copy)
        case MouseDown(p, _, _) :: tail => {
          if (Model.isSelected) {
            shapes = shapes ++ Model.selected
          } else if (Model(p).isDefined) {
            shapes = Seq(Model(p).get)
            closeToObjectOnStart = true
          } else Goto('End)// If there aren't any active, quit the module
          basePoint = Some(p)
        }
        case MouseDrag(_, _, _) :: MouseDown(p, _, _) :: tail => {
          if (Model.isSelected) {
            shapes = shapes ++ Model.selected
          } else if (Model(p).isDefined) {
            val closestShape = Model(p).get
          } else Goto ('End)// If there aren't any active, quit the module
          basePoint = Some(p)
        }
        case _ =>
      }
      if (basePoint.isEmpty) Goto('End)
      None
    }),
    'Move -> ((events : List[Event]) => {
      events match {
        case MouseDrag(p, _, _) :: tail => {
          endPoint = Some(p)
        }
        case _ =>
      }
      None
    }),
    'End   -> ((events : List[Event]) => {
      doneDrawing = true
      events match {
        case MouseUp(p, _, _) :: tail => {
          if (delta.isDefined && delta.get.length > 0)
            Transform(shapes, TransformationMatrix(delta.get, 1))
        }
        case _ =>
      }
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (delta.isDefined && !doneDrawing) {
      shapes.foreach(s => {
        g draw s.transform(TransformationMatrix(delta.get, 1)).transform(t)
      })
    }
  }

}
