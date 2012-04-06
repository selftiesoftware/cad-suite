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
import module.base.create.{PointGuide, AngleSnap}

object Move extends Module {

  private var endPoint : Option[Vector2D] = None
  private var movedShape : LineShape = (LineShape(Vector2D(0,0),Vector2D(0,100)).addAttributes("Color" -> "#AAAAAA".color))
  private var moveVector : Option[Vector2D] = None
  private var testShape : LineShape = (LineShape(Vector2D(0,0),Vector2D(0,100)).addAttributes("Color" -> "#AAAAAA".color))
  private var startPoint : Option[Vector2D] = None
  private var startPointSet = false
  private var transformation = new TransformationMatrix()

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start      -> 'KeyEscape -> 'End)
  
  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      val shapeGuide : Vector2D => LineShape = (v : Vector2D) => {
        // Create a matrix
        val t : TransformationMatrix = if (startPoint.isDefined) {
          TransformationMatrix(v, 1)
        // If no startpoint has been defined - create empty matrix
        } else TransformationMatrix()
        // Return the shape, transformed
        testShape.transform(t)
      }
      //if the start point has not been set, then set it:
      if(!startPoint.isDefined){
        Siigna.display("Select a point to move from")
        events match{
          //if a startPoint is received from Point:
          case Message(p : Vector2D) :: MouseDown(_ ,_ ,_) :: tail => {
            //set the point as start point
            startPoint = Some(p)
            //send a shapeGuide to Point to draw the moving object(s) dynamically
              println("sending shapeGuide")
              Send(Message(PointGuide(shapeGuide)))
            //and goto Point to get the moveVector
            ForwardTo('Point, false)
          }
          //exit mechanisms
          case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

          //disregard mouse moves
          case MouseMove(p ,_ ,_) :: tail =>
          //if the mouse is pressed, forward to Point to check if the user calls the Angle Gizmo.
          case MouseDown(p, _, _) :: tail => {
            ForwardTo('Point)
          }
          case _ =>
        }
      // if the start point and move point is set, goto End.
      }
      else if(startPoint.isDefined){
        events match{
          case Message(p : Vector2D) :: MouseDown(_ ,_ ,_) :: tail => {
            endPoint = Some(p)
            moveVector = Some(endPoint.get - startPoint.get)
            println("going to END")
            Goto('End)
          }
          case _ => {
            ForwardTo('Point, false)
          }
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      println("in END")
      events match {
        case Message(p : Vector2D) :: tail => {
          movedShape = testShape.transform(transformation)
          Create(movedShape)
        }
        case _ => {
          Goto('End, false)
        }
      }
    //clear vars
     endPoint = None
     moveVector = None
     startPointSet = false
     startPoint = None

    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if(!startPoint.isDefined){
      g draw testShape.transform(t)
    } else if(startPoint.isDefined){
      g draw testShape.transform(t)
      g draw (CircleShape(startPoint.get,(startPoint.get + Vector2D(0,3)))).transform(t)

    }
  }
}