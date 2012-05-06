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
import module.base.create.{PointGuides, PointGuide, AngleSnap}

// TODO: add object selection logic.
/**
 * A module that rotates one or more objects.
 */
object Rotate extends Module {

  private var firstMouseDown = false
  private var centerPoint : Option[Vector2D] = None
  private var endVector : Option[Vector2D] = None
  private var rotation : Double = 0
  private var startVector : Option[Vector2D] = None
  private var startVectorSet = false
  private var transformation : Option[TransformationMatrix] = None


  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start       -> 'KeyEscape -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //a guide to get Point to dynamically draw the shape(s) and their rotation
      val shapeGuide : Vector2D => Traversable[Shape] = (v : Vector2D) => {
        // Create a matrix
        val t : TransformationMatrix = if (startVector.isDefined && centerPoint.isDefined) {
          // To find the angle between two vectors (lines)
          // First calculate the separate angles
            val a1 : Double = (startVector.get - centerPoint.get).angle
            val a2 : Double = (v - centerPoint.get).angle
          // ... And then subtract the second from the first
            TransformationMatrix(Vector2D(0,0), 1).rotate(a2 - a1, centerPoint.get)
        // If no start- (or center-) point has been defined - create empty matrix
        } else TransformationMatrix()
        // Return the shape, transformed
        //Model.selection.get.apply(t)
        Model.selection.get.apply(t)
      }

      //if the center for the rotation has not been set, then set it:
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
              println("setting center now")
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
            //send the selected shapes as a guide to 'Point to draw them dynamically while waiting for the end Vector.
            Controller ! Message(PointGuides(shapeGuide))
            startVector = Some(p)
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
        def getEndVector(p : Vector2D) = {
          endVector = Some(p)
         (p - startVector.get)
        }
         val translation = events match {
          case MouseDown(p, _, _) :: tail => getEndVector(p)
          case MouseDrag(p, _, _) :: tail => getEndVector(p)
          case MouseMove(p, _, _) :: tail => getEndVector(p)
          case MouseUp(p, _, _) :: tail => {
            getEndVector(p)
          }
          case _ => Vector2D(0, 0)
        }

        events match{

          case Message(p : Vector2D) :: MouseDown(_ ,_ ,_) :: tail => {
            endVector = Some(p)
            rotation = ((startVector.get - centerPoint.get) - (endVector.get - centerPoint.get)).angle
            transformation = Some(TransformationMatrix(translation, 1))
            println("ANGLE: " +rotation)
            Goto('End)
          }
          case _ => {
            ForwardTo('Point, false)
          }
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {

          //do the rotation:
            val t : TransformationMatrix = {
            val a1 : Double = (startVector.get - centerPoint.get).angle
            val a2 : Double = (endVector.get - centerPoint.get).angle

            TransformationMatrix(Vector2D(0,0), 1).rotate(a2 - a1, centerPoint.get)
          }
          //val t = transformation.get.rotate(rotation,centerPoint.get)
          Model.selection.get.transform(t)
          Model.deselect()
        }
        case _ => {
          Goto('End, false)
        }
      }
      //clear vars
      com.siigna.module.base.Default.previousModule = Some('Rotate)
      centerPoint = None
      endVector = None
      firstMouseDown = false
      rotation = 0
      startVectorSet = false
      startVector = None
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    println("CENTER POINT: "+centerPoint)

    Model.selection.foreach(s => transformation.foreach(s.apply(_).foreach(s => g.draw(s.transform(t)))))

    if(startVector.isDefined) g draw (CircleShape(startVector.get,(startVector.get + Vector2D(0,9)))).transform(t)
    else if(centerPoint.isDefined && !startVector.isDefined) {
      g draw (CircleShape(startVector.get,(startVector.get + Vector2D(10,0)))).transform(t)
      g draw (CircleShape(centerPoint.get,(centerPoint.get + Vector2D(10,0)))).transform(t)
    }
  }
}