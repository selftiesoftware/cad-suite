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
  private var centerPoint : Option[Vector2D] = None
  private var endVector : Option[Vector2D] = None
  private var rotation : Double = 0
  //a line to test rotation before selection is implemented and it is possible to use the selection module to select shapes to rotate:
  private var testShape : LineShape = (LineShape(Vector2D(0,0),Vector2D(0,100)).addAttributes("Color" -> "#AAAAAA".color))
  private var rotatedShapes : List[Shape] = List()
  private var startVector : Option[Vector2D] = None
  private var startVectorSet = false
  private var transformation = TransformationMatrix


  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start       -> 'KeyEscape -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //a guide to get Point to dynamically draw the shape(s) and their rotation
      val shapeGuide : Vector2D => LineShape = (v : Vector2D) => {
        testShape
        //testShape.transform(transformation.rotate(20,centerPoint.get))

      }


      //if the center has not been set, then set it:
      if(!centerPoint.isDefined){
        Siigna.display("Select a base point for the rotation")
        events match{
          //exit mechanisms
          case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

          //disregard mouse moves
          case MouseMove(p ,_ ,_) :: tail =>
          //if the mouse is pressed, forward to Point to check if the user calls the Angle Gizmo.
          case MouseDown(p, _, _) :: tail => {
            if(firstMouseDown == false)
              firstMouseDown = true
            else {
              println("got center point. Set StartAngle")
              centerPoint = Some(p)
              ForwardTo('Point, false)
            }
          }
          case _ =>
        }
      }
      //if the start angle has not been set, then set it:
      else if(!startVector.isDefined){
        events match{
          case Message(p : Vector2D) :: MouseDown(_ ,_ ,_) :: tail => {
            Send(Message(PointGuide(shapeGuide)))
            startVector = Some(p)
            println("got startAngle. rotation: "+rotation)
            ForwardTo('Point)
          }
          case _ => {
            Siigna.display("Select a starting point for the rotation")
            ForwardTo('Point, false)
          }
        }
      }
      //if both a center and a startAngle is set, set the final point of the rotation.
      else if(centerPoint.isDefined && startVector.isDefined){
        events match{
          case Message(p : Vector2D) :: MouseDown(_ ,_ ,_) :: tail => {
            println("got endAngle: "+endVector)
            endVector = Some(p)
            rotation = (endVector.get - startVector.get).angle
            Goto('End)
          }
          case _ => {
            println("start angle set, set end point.")
            ForwardTo('Point, false)
          }
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          //ROTATE THE SHAPE HERE
        }
        case _ => {
          Goto('End, false)
        }
      }
      //clear vars
      centerPoint = None
      endVector = None
      firstMouseDown = false
      rotation = 0
      startVectorSet = false
      startVector = None
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
     g draw testShape.transform(t)

    if(centerPoint.isDefined && !startVector.isDefined){
      g draw testShape.transform(t.rotate(rotation, centerPoint.get))
      g draw (CircleShape(centerPoint.get,(centerPoint.get + Vector2D(0,3)))).transform(t)
    }
  }
}