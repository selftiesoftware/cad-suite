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

object Scale extends Module {

  var endPoint : Option[Vector2D] = None

  var ending : Boolean = false

  var gotEndPoint : Boolean = false

  var scaleFactor : Option[Vector2D] = None

  //a guide to get Point to draw the shape(s) dynamically
  val shapeGuide : Vector2D => Traversable[Shape] = (v : Vector2D) => {
    // Create a matrix
    val t : TransformationMatrix = if (startPoint.isDefined) {
      TransformationMatrix(v - startPoint.get, 1)
    // If no startPoint has been defined - create an empty matrix
    } else TransformationMatrix()
    // Return the shape, transformed
    Model.selection.get.apply(t)
  }

  var startPoint : Option[Vector2D] = None

  var transformation : Option[TransformationMatrix] = None

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start -> 'KeyDown -> 'End,
    'Move  -> 'KeyDown -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //start 'Scale only if there is a selection
      if (!Model.selection.isEmpty) {
        events match {
          case Message(p : Option[Vector2D]) :: tail => startPoint = p
          case MouseDown(p, MouseButtonLeft, _) :: tail => startPoint = Some(p)
          case MouseDown(p, MouseButtonRight, _) :: tail => ForwardTo('Menu)
          case MouseMove(p, _, _) :: tail => startPoint = Some(p)
          case MouseDrag(p, _, _) :: tail => {

            startPoint = Some(p)
              if (Model.selection.isDefined && startPoint.isDefined) {
                Goto('Move)
              } else {
                Goto('End)
            }
          }
          case MouseUp(p, _,_) :: MouseDown(_, _, _) :: tail => Goto('End)
          case _ =>
        }
      }
      // if no selection is made, go to the selection module
      else {
        Siigna display "Select objects to scale"
        Goto('End)
      }
    }),
    'StartPoint ->   ((events : List[Event]) => {
      Siigna display "set startpoint"
      events match {
        case Message(p : Vector2D) :: tail => {
          startPoint = Some(p)
          Goto('EndPoint)
        }
        case MouseUp(p, _, _) :: MouseDown(_ ,_ ,_) :: tail => {
          ForwardTo('Point)
          Controller ! Message(PointGuides(shapeGuide))
        }
        case _ =>
      }
    }),
    'EndPoint ->   ((events : List[Event]) => {
      Siigna display "set endpoint"
      events match {
        case Message(p : Vector2D) :: tail => {
          endPoint = Some(p)
          Goto('Scale)
        }
        case MouseUp(p, _, _) :: MouseDown(_ ,_ ,_) :: tail => {
          ForwardTo('Point)
          Controller ! Message(PointGuides(shapeGuide))
        }
        case _ =>
      }
    }),
    'Scale -> ((events : List[Event]) => {
      Siigna display "set scale factor"
      var refScale : Vector2D = startPoint.get - endPoint.get

      def getScaleFactor (p : Vector2D) = {
        endPoint = Some(p)
        (p - startPoint.get).length/refScale.length
      }
      //if scaling is performed with the mouse:
      if (startPoint.isDefined) {
        val translation = events match {
          case MouseDown(p, _, _) :: tail => getScaleFactor(p)
          case MouseDrag(p, _, _) :: tail => getScaleFactor(p)
          case MouseMove(p, _, _) :: tail => getScaleFactor(p)
          case MouseUp(p, _, _) :: tail => {
            ending = true
            getScaleFactor(p)
          }
          case _ => Vector2D(0, 0)
        }
        //TODO: if else hack to bypass unability to add Goto('End) in case MouseUp above (since it needs to return a value). Adding MouseUp -> 'End in the stateMap will cause the module to crach when double clicking.
        if (ending == false) {
          //transformation = Some(TransformationMatrix(translation, 1))
          //Model.selection.get.transform(transformation.get)
        } else {
          //transformation = Some(TransformationMatrix(translation, 1))
          //Model.selection.get.transform(transformation.get)
          Goto('End)
        }
      }
      //if moving is performed with a module call from the menu:
      else if (startPoint.isDefined) {
        //check if the endPoint is set. If not, goto 'Point.
        if (gotEndPoint == false) {
          gotEndPoint = true
          ForwardTo('Point)
        }
        //if the message arrives after the gotEndPoint flag is set, use it to define the endpoint:
        //TODO: this is a hack, could probably be made alot nicer...
        else if(gotEndPoint == true) {
          events match {
            case Message (p : Vector2D) :: tail => {
              //var oldShapes:Map[Int,Shape] = Map()
              //Model.selection.get.shapes.foreach(tuple => {
              //  oldShapes += tuple
              //})
              transformation = Some(TransformationMatrix((p - startPoint.get), 1))
              Model.selection.get.transform(transformation.get)
              //Model.selection.get.shapes.foreach(tuple => {
              //  UpdateShape(AppletParameters.getDrawingId.get, tuple._1, oldShapes(tuple._1), tuple._2, AppletParameters.getClient)
              //})
              Model.deselect()
              Goto('End)
            }
            case _ => None
          }
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      //deselect, but only if an objects has been moved.
      if (Model.selection.isDefined && startPoint.isDefined && endPoint.isDefined && (startPoint.get - endPoint.get != Vector2D(0, 0))) {
        Model.deselect()
      }
      //clear the vars
      com.siigna.module.base.Default.previousModule = Some('Move)
      ending = false
      gotEndPoint = false
      startPoint = None
      endPoint = None
      transformation = None
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    Model.selection.foreach(s => transformation.foreach(s.apply(_).foreach(s => g.draw(s.transform(t)))))
  }

}