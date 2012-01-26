/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna.module.Module

import com.siigna._

object Move extends Module {

  var basePoint : Option[Vector2D] = None
  var endPoint  : Option[Vector2D] = None
  var doneDrawing = false
  var closeToObjectOnStart = false

  def delta : Option[Vector2D] = if (displacement.isDefined && basePoint.isDefined) Some(displacement.get - basePoint.get) else None

  var displacement : Option[Vector2D] = None

  var shapes : Seq[Shape] = Seq()

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End,
                                        'Start     -> 'MouseDrag -> 'Move,
                                        'Move      -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      if(Model.isSelected) {
        Siigna display("Select point to move from")
        Goto('SetOrigin, false)
      } else {
        Siigna display("Select objects to move")
        ForwardTo('Select)
      }
    }),
    'SetOrigin -> ((events : List[Event]) => {
      //TODO: refactor. -this is a hack to make sure 'Point gets the guide it needs.
      val moveGuide : Vector2D => CircleShape = (v : Vector2D) => {
      CircleShape(v, v + Vector2D(0,4))
      }
      events match {
        case KeyDown(Key.Control, _) :: tail => Goto('End); ForwardTo('Copy)
        case MouseMove(p, _, _) :: tail => {
          //Send(Message(PointGuide(moveGuide)))
          ForwardTo('Point, false)
        }
        case Message(p : Vector2D) :: tail => basePoint = Some(p)
          println("GOT POINT MESSAGE IN SETSTART")

          if (Model.isSelected) {
            shapes = shapes ++ Model.selected
          } else if (Model(p).isDefined) {
            shapes = Seq(Model(p).get)
            closeToObjectOnStart = true
            Goto('Move)
          } else {
            Goto('End)
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
    'SetDestination -> ((events : List[Event]) => {
      println("in move")
      events match {
        case MouseMove(p, _, _) :: tail => displacement = Some(p)
        case MouseUp(p, _, _) :: tail => {
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
