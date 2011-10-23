/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  var points   = List[Vector]()
  var shape : PolylineShape = PolylineShape.empty

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (shape.shapes.size > 0) g draw shape.transform(t)
  }

  def stateMap = DirectedGraph(
    'Start         -> 'Action    -> 'GotPoint,
    'Start         -> 'KeyEscape -> 'End,
    'GetPoint      -> 'Action    -> 'GotPoint,
    'GotPoint      -> 'KeyEscape -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseDown(_, _, ModifierKeys(false, true, false)) :: tail => {
          ForwardTo('Arc)
        }
        case MouseUp(_, _, ModifierKeys(false, true, false)) :: tail => {
          ForwardTo('Arc)
        }
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ => {
          if (points.lengthCompare(0) > 0) {
            ForwardTo('Point)
            Message(PointShape(points.head))
          } else if (shape.shapes.size > 0) {
            ForwardTo('Point)
            Message(shape.shapes.head)
          } else
            ForwardTo('Point)
        }
      }
    }),
    'GetPoint -> ((events : List[Event]) => {
      events match {
       // case KeyDown(Key.Enter | Key.Space, _) :: tail => goto('End)
       // case _ :: KeyDown(Key.Enter | Key.Space, _) :: tail => goto('End)
        case MouseMove(_,_,_) :: MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ => {
          ForwardTo('Point)
        }
      }
      None
    }),
    'GotPoint -> ((events : List[Event]) => {
      println("Got point: "+events)
      events match {
        case Message(_) :: MouseDown(_, MouseButtonRight, _) :: tail => Goto('End)
        case Message(point : PointShape) :: tail => {
          if (points.length == 1) {
            shape = shape.+:(LineShape(points(0), point.point))
            points = List(point.point)
          } else if (points.length == 0) {
            points = point.point :: points
          } else points = List()
          Goto('GetPoint)
        }
        case Message(arc : ArcShape) :: tail => {
          shape = shape.+:(ArcShape(arc.start, arc.middle, arc.end))
          points = List(arc.end)
        }
        case _ => {
          Goto('End)
        }
      }
    }),
    'End -> ((events : List[Event]) => {
      // Clear the module-variables
      points = Nil
      val finishedShape = shape
      shape = PolylineShape.empty

      events match {
        case KeyDown(Key.Escape, _) :: tail => None
        case _ => {
          if (finishedShape.shapes.size > 0) {
            Create(finishedShape)
          }
        }
      }
    })
  )

}
