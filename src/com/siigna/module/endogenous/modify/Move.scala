/*
package com.siigna.module.endogenous.modifyCategory

/* 2010 (C) Copyright by Siigna, all rights reserved. */

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
                                   //'Start     -> 'MouseDrag -> 'Move,
                                   'Move      -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      println("EVENTS IN START MOVE: "+events.head)
      events match {
        //if 'Point returns a point and no base is set, then set it.
        case Message(p : Vector2D) :: tail =>
        case _ => {
          println("START in MOVE")
          if(Model.isSelected) {
            //Goto('Move, false)
          } else {
            Siigna display("Select objects to move")
            println("GOING TO SELECTION")
            //ForwardTo('Selection, false)
          }
        }
      }
    }),
    'Move -> ((events : List[Event]) => {
      //this is a hack to make sure 'Point gets the guide it needs.
      Siigna.display("set point to move from")
      val moveGuide : Vector2D => CircleShape = (v : Vector2D) => {
      CircleShape(v, v + Vector2D(0,4))
      }
      events match {
        //case KeyDown(Key.Control, _) :: tail => Goto('End); ForwardTo('Copy)
        case MouseDown(p, _, _) :: tail => {
          println("got MD in move")
          //Send(Message(PointGuide(moveGuide)))
          //ForwardTo('Point)
        }
        case Message(p : Vector2D) :: tail => {
          basePoint = Some(p)
          if(!basePoint.isDefined) {
            basePoint = Some(p)
            //ForwardTo('Point)
          }
          //if the base is already set, it means the point is the displacement.
          else {
            displacement = Some(p)
            //Goto('End)
          }
        }

  //        if (Model.isSelected) {
  //          shapes = shapes ++ Model.selected
  //        } else if (Model(p).isDefined) {
  //          shapes = Seq(Model(p).get)
  //          closeToObjectOnStart = true
  //          Goto('Move)
  //        } else {
  //          Goto('End)
  //          basePoint = Some(p)
  //      }
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


*/