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

/*package com.siigna.module.base.modify

import com.siigna._

class Move extends Module {

  var endPoint : Option[Vector2D] = None

  //a guide to get Point to draw the shape(s) dynamically
  val shapeGuide : Vector2D => Traversable[Shape] = (v : Vector2D) => {
    // CreateCategory a matrix
    val t : TransformationMatrix = if (startPoint.isDefined) {
      TransformationMatrix(v - startPoint.get, 1)
    // If no startPoint has been defined - create an empty matrix
    } else TransformationMatrix()
    // Return the shape, transformed
    Drawing.selection.get.apply(t)
  }

  var startPoint : Option[Vector2D] = None
  
  var transformation : Option[TransformationMatrix] = None
  
  lazy val stateMachine = Map(
    'Start -> {
      case Start(_, p : Vector2D) :: tail => {
        startPoint = Some(p)
        'Delta
      }
      case Start(_, s : Selection) :: tail => {

      }
    }
    'StartXX -> ((events : List[Event]) => {
      //start 'Move only if there is a selection
      if (!Drawing.selection.isEmpty) {
        if(moduleCallFromMenu == true) Goto('StartPoint, false)
        else {
          events match {
            case Message(p : Option[Vector2D]) :: tail => startPoint = p
            case MouseDown(p, MouseButtonLeft, _) :: tail => startPoint = Some(p)
            case MouseDown(p, MouseButtonRight, _) :: tail => Module('Menu)
            case MouseMove(p, _, _) :: tail => startPoint = Some(p)
            case MouseDrag(p, _, _) :: tail => {

              startPoint = Some(p)
                if (Drawing.selection.isDefined && startPoint.isDefined) {
                  Goto('Move)
                } else {
                  Goto('End)
              }
            }
            //set startPoint conditions : 1) shape selected. 2) mouseUp 3) Move selected from the menu / shortcut
            //goto End contitions: 1) shape selected 2) mouseUp 3) move called from selection
            case MouseUp(p, _,_) :: tail => Goto('End)
            //moving with keys
            //case KeyDown(Key.arrowLeft, _) :: tail => {
            //  println("left arrow")
            //}
            //catch 'MM' commands
            case KeyUp(_, _) :: tail => {
              moduleCallFromMenu = true
              Goto('StartPoint, false)
            }
            case _ =>
          }
        }
      }
      // if no selection is made, go to the selection module
      else {
        Siigna display "Select objects to move"
        Goto('End)
      }
    }),
    'StartPoint ->   ((events : List[Event]) => {
      Siigna display "set base point"
      events match {
        case Message(p : Vector2D) :: tail => {
          startPoint = Some(p)
          Goto('Move)
        }
        case _ => {
          com.siigna.module.base.ModuleInit.previousModule = Some('Move)
          Module('Point)
          Controller ! Message(PointGuides(shapeGuide))
        }
      }
    }),
    'Move -> ((events : List[Event]) => {
      events match{
        case MouseDown(_, MouseButtonRight, _) :: tail => {
          Goto('End, false)
        }
        case _ => {
          def getEndPoint(p : Vector2D) = {
            endPoint = Some(p)
            (p - startPoint.get)
          }
          //if moving is initiated and completed by dragging the mouse:
          if (startPoint.isDefined && moduleCallFromMenu == false) {
            val translation = events match {
              case MouseDown(p, _, _) :: tail => getEndPoint(p)
              case MouseDrag(p, _, _) :: tail => getEndPoint(p)
              case MouseMove(p, _, _) :: tail => getEndPoint(p)
              case MouseUp(p, _, _) :: tail => {
                ending = true
                getEndPoint(p)
              }
              case _ => Vector2D(0, 0)
            }
            //TODO: if else hack to bypass unability to add Goto('End) in case MouseUp above (since it needs to return a value). Adding MouseUp -> 'End in the stateMap will cause the module to crach when double clicking.
            if (ending == false) {
              transformation = Some(TransformationMatrix(translation, 1))
              Drawing.selection.get.transform(transformation.get)
            } else {
              transformation = Some(TransformationMatrix(translation, 1))
              Drawing.selection.get.transform(transformation.get)
              Goto('End)
            }
          }
          //if moving is performed with a module call from the menu:
          else if (startPoint.isDefined && moduleCallFromMenu == true) {
            //check if the endPoint is set. If not, goto 'Point.
            if (gotEndPoint == false) {
              gotEndPoint = true
              Module('Point)
            }
            //if the message arrives after the gotEndPoint flag is set, use it to define the endpoint:
            //TODO: this is a hack, could probably be made alot nicer...
            else if(gotEndPoint == true) {
              events match {
                //if an EndPoint is returned from 'Point:
                case Message (p : Vector2D) :: tail => {
                  //var oldShapes:Map[Int,Shape] = Map()
                  //Drawing.selection.get.shapes.foreach(tuple => {
                  //  oldShapes += tuple
                  //})
                  transformation = Some(TransformationMatrix((p - startPoint.get), 1))
                  Drawing.selection.get.transform(transformation.get)
                  //Drawing.selection.get.shapes.foreach(tuple => {
                  //  UpdateShape(AppletParameters.getDrawingId.get, tuple._1, oldShapes(tuple._1), tuple._2, AppletParameters.getClient)
                  //})
                  Drawing.deselect()
                  Goto('End)
                }
                case _ => None
              }
            }
          }
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      //deselect, but only if an objects has been moved.
      //if (Drawing.selection.isDefined && startPoint.isDefined && endPoint.isDefined && (startPoint.get - endPoint.get != Vector2D(0, 0))) {
      if (Drawing.selection.isDefined && startPoint.isDefined && endPoint.isDefined) {

        Drawing.deselect()
      }
    })
  )

  //draw the moving geometry when dragging the mouse
  override def paint(g : Graphics, t : TransformationMatrix) {
    Drawing.selection.foreach(s => transformation.foreach(s.apply(_).foreach(s => g.draw(s.transform(t)))))
  }

}*/