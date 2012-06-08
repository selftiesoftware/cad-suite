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


object Scale extends Module {

  //TODO: scale the object FROM around the StartPoint not CENTERED around the StartPoint
  //VARIABLES:
  var ending : Boolean = false
  var endPoint : Option[Vector2D] = None
  var gotEndPoint : Boolean = false
  var scaleFactor : Option[Vector2D] = None
  //a guide to get Point to draw the shape(s) dynamically
  val shapeGuide : Vector2D => Traversable[Shape] = (v : Vector2D) => {

    val refScale : Vector2D = startPoint.get - endPoint.get
    var scaleFactor = ((v - startPoint.get).length/refScale.length).toDouble
    // Create a matrix
    val t : TransformationMatrix = if (startPoint.isDefined) {
      TransformationMatrix(Vector2D(0,0), 1).scale(scaleFactor,startPoint.get)
    // If no startPoint has been defined - create an empty matrix
    } else TransformationMatrix()
    // Return the shape, transformed
    Model.selection.get.apply(t)
  }

  var startPoint : Option[Vector2D] = None
  var text = ""
  var transformation : Option[TransformationMatrix] = None
  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start -> 'KeyDown -> 'End,
    'EndPoint  -> 'KeyDown -> 'End,
    'Scale  -> 'KeyDown -> 'End
  )

  lazy val stateMachine = Map(
    'Start ->   ((events : List[Event]) => {
      if (Model.selection.isDefined) {
        Siigna display "set startpoint"
        events match {
          case Message(p : Vector2D) :: tail => {
            startPoint = Some(p)
            Goto('EndPoint, false)
          }
          case MouseUp(p, _, _) :: MouseDown(_ ,_ ,_) :: tail => {
            ForwardTo('Point)
          }
          case _ =>
        }
      }
      // TODO: add object selection logic.
      else Siigna display "Select objects first"
    }),
    'EndPoint ->   ((events : List[Event]) => {
      Siigna display "set endpoint"
      events match {
        case Message(p : Vector2D) :: tail => {
          endPoint = Some(p)
          Siigna display "type or click to set scale factor"
          Goto('TextInput)
        }
        case MouseUp(p, _, _) :: MouseDown(_ ,_ ,_) :: tail => {
          ForwardTo('Point)
          Controller ! Message(PointGuides(shapeGuide))
        }
        case _ =>
      }
    }),
    //TODO: find a way to allow textInput after Mouse Moves in 'Scale
    'TextInput -> ((events : List[Event]) => {
      events match {
        case MouseMove (p, _, _) :: tail => Goto('Scale)
        case KeyDown(Key.Backspace, _) :: tail => {
           if (text.length != 0) text = text.substring(0, text.length - 1)
           else Goto('End)
        }
        case KeyDown(Key.Enter, _) :: tail => {
          var numbers = text.toDouble
          transformation = Some(TransformationMatrix(Vector2D(0,0),1))
          //perform scaling:
          //Model.selection.get.transform(transformation.get.scale(numbers))
          Model.selection.get.transform(transformation.get.scale(numbers,startPoint.get))

          text = ""

          Goto('End)
        }
        case KeyDown(Key.Esc, _) :: tail => {
          text = ""
          Goto('End)
        }
        case KeyDown(key, _) :: tail => {
          text += key.toChar.toString.toLowerCase
          Siigna display text
        }
        case _ => None
      }
    }),
    'Scale -> ((events : List[Event]) => {
      //check if the endPoint is set. If not, goto 'Point.
      if (gotEndPoint == false) {
        gotEndPoint = true
        ForwardTo('Point)
      }
      //if the message arrives after the gotEndPoint flag is set, use it to define the endpoint:
      //TODO: this is a hack, could probably be made alot nicer...
      else if(gotEndPoint == true) {
        events match {
          case MouseMove (p, _, _) :: tail =>
          case Message (p : Vector2D) :: tail => {
            //var oldShapes:Map[Int,Shape] = Map()
            //Model.selection.get.shapes.foreach(tuple => {
            //  oldShapes += tuple
            //})

            val refScale : Vector2D = startPoint.get - endPoint.get
            var scaleFactor = ((p - startPoint.get).length/refScale.length).toDouble
            transformation = Some(TransformationMatrix(Vector2D(0,0),1))
            Model.selection.get.transform(transformation.get.scale(scaleFactor,startPoint.get))
            //Model.selection.get.shapes.foreach(tuple => {
            //  UpdateShape(AppletParameters.getDrawingId.get, tuple._1, oldShapes(tuple._1), tuple._2, AppletParameters.getClient)
            //})
            Model.deselect()
            Goto('End)
          }
          case _ => None
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      //deselect, but only if an objects has been moved.
      if (Model.selection.isDefined && startPoint.isDefined && endPoint.isDefined && (startPoint.get - endPoint.get != Vector2D(0, 0))) {
        Model.deselect()
      }
      //clear the vars
      com.siigna.module.base.Default.previousModule = Some('Scale)
      ending = false
      gotEndPoint = false
      println("startPoint in End: "+startPoint)
      startPoint = None
      endPoint = None
      transformation = None
    })
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    Model.selection.foreach(s => transformation.foreach(s.apply(_).foreach(s => g.draw(s.transform(t)))))
  }

}