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
import module.base.create.{PointGuide, AngleSnap}

// TODO: add object selection logic.

object Rotate extends Module {

  private var firstMouseDown = false
  private var startAngle : Option[Vector2D] = None
  private var centerPoint : Vector2D = Vector2D(0,0)
  private var startVector : Option[Vector2D] = None
  private var startVectorSet = false
  private var transformation = TransformationMatrix()


  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start       -> 'KeyEscape -> 'End,
    //'Start       -> 'Message   -> 'CenterPoint,
    'CenterPoint -> 'Message   -> 'StartAngle
    //'StartAngle  -> 'Message   -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      println("ROTATE, start, e: "+events)
      def centerGuide = (p : Vector2D) => CircleShape(centerPoint,(centerPoint + Vector2D(0,3)))

      //println(events)
      Siigna.display("Select a base point for the rotation")
      events match{
        //exit mechanisms
        case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

        //match returns from 'Point with centerPoint
        case Message(p : Vector2D) :: MouseDown(_ ,_ ,_) :: tail => {
          startAngle = Some(p)
          //Goto('CenterPoint) nej, dette STOPPER POINT!
        }

        //disregard mouse moves
        case MouseMove(p ,_ ,_) :: tail =>
        //if the mouse is pressed, forward to Point to check if the user calls the Angle Gizmo.
        case MouseDown(p, _, _) :: tail => {
          centerPoint = (p)
          if(firstMouseDown == false)
            firstMouseDown = true
          else {
            Send(Message(centerPoint))
            Send(Message(PointGuide(centerGuide)))
            println("in RT start, going to Point")
            ForwardTo('Point)
          }

        }
        case _ =>
      }
    }),
    'StartAngle   -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          if (startVectorSet == true) {
            startVector = Some(p)
            Siigna.display("click to set rotation")
            println("GOT STARTVECTOR: "+startVector.get)
            ForwardTo('Point, false)
          } else {
            startVectorSet = true
            ForwardTo('Point, false)
          }
        }
        case _ => {
          Goto('End, false)
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          println("centerPoint in End: "+centerPoint)
        }
        case _ => {
          Goto('End, false)
        }
      }
      //clear vars
      firstMouseDown = false
      startVectorSet = false
      centerPoint = Vector2D(0,0)
      startAngle = None
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
     // g draw (CircleShape(centerPoint,(centerPoint + Vector2D(0,3)))).transform(t)
  }

}