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

package com.siigna.module.base.create

import com.siigna._
import app.Siigna
import com.siigna.module.Module

import java.awt.Color

class Fill extends Module {

  var startPoint: Option[Vector2D] = None
  private var points   = List[Vector2D]()
  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, 1.00f)
  var attributes : Attributes = Attributes()
  def set(name : String, attr : String) = Siigna.get(name).foreach((p : Any) => attributes = attributes + (attr -> p))

  // The polylineshape so far
  private var shape : Option[PolylineShape] = None

  val stateMap: StateMap = Map(
    'Start -> {
      case End(v : Vector2D) :: tail => {
        //if the point module returns with END and a point, a new point is received.
        points = points :+ v
        if (startPoint.isEmpty){
          //If the start point is not yet set, then the first segment is being drawn, which means a guide can be made.
          startPoint = Some(v)

          val guide = PointPointGuide(v, (v : Vector2D) => {
            //(Array(PolylineShape(startPoint.get, v)))
            if(!points.isEmpty) {
              val closedPolyline = points.reverse :+ v
              val areaGuide = closedPolyline.reverse :+ v
              if(Siigna.get("activeColor").isDefined) {
                Array(PolylineShape((areaGuide)).setAttributes("raster" -> Siigna("activeColor"), "StrokeWidth" -> 0.0))
              }
              else Array(PolylineShape((areaGuide)).setAttributes("raster" -> anthracite, "StrokeWidth" -> 0.0))
            }
            else Array(PolylineShape(Vector2D(0,0),Vector2D(0,0)))
          },1)//1 : Input type = InputTwoValues

          Start('Input,"com.siigna.module.base.create", guide)
        } else {
          //If the start point is set, the first segment is made and points should be added.
          points :+ v
          //val guide : Guide = Guide((v : Vector2D) => {
          //  Array(PolylineShape(points :+ v))
          //})
          val guide = PointPointGuide(v, (v : Vector2D) => {
            (Array(PolylineShape(points :+ v)))
          },1)//1 : Input type = InputTwoValues
          Start('Input,"com.siigna.module.base.create", guide)
        }
      }

      //If input module does not return any input:
      case End("no point returned") :: tail => {
        //If there only is the start point:
        if (points.length == 1){
          //If the start point is not yet set, then the first segment is being drawn, which means a guide can be made.
          startPoint = Some(points.last)

          val guide = PointPointGuide(startPoint.get, (v : Vector2D) => {
            (Array(PolylineShape(startPoint.get, v)))
          },1)//1 : Input type = InputTwoValues

          Start('Input,"com.siigna.module.base.create", guide)
        } else {

          val guide = PointPointGuide(points.last, (v : Vector2D) => {
            (Array(PolylineShape(points :+ v)))
          },1)//1 : Input type = InputTwoValues
          Start('Input,"com.siigna.module.base.create", guide)
        }
      }

      //If point module returns a key-pres at the event when it ends:
      case End(k : KeyDown) :: tail => {
        // If the key is backspace without modification (shift etc), the last bit of line is deleted:
        if (k == KeyDown(Key.Backspace,ModifierKeys(false,false,false))) {
          if (points.length > 1) {
            points = points.dropRight(1)
          }
          //And if there is a start point, a new guide is returned
          if (startPoint.isDefined) {
            val guide : PointPointGuide = PointPointGuide(points.last, (v : Vector2D) => {
              Array(PolylineShape(points :+ v))
            },1) //1 : Input type = InputTwoValues
            Start('Input,"com.siigna.module.base.create", guide)
          } else {
            //If not, point is started without guide.
            Start('Input,"com.siigna.module.base.create")
          }
        }}

      case End(MouseDown(p, MouseButtonRight, _)) :: tail => {

        var plShape = PolylineShape(points)
        def setAttribute[T : Manifest](name:String, shape:Shape) = {
          Siigna.get(name) match {
            case s : Some[T] => shape.addAttribute(name, s.get)
            case None => shape// Option isn't set. Do nothing
          }
        }
        val polyLine = setAttribute[Color]("Color",setAttribute[Double]("LineWeight", plShape))
        Create(polyLine)
        End
      }

      case End :: tail => {
        //If there are two or more points in the polyline, it can be saved to the Siigna universe.
        if (points.length > 1) {
          var plShape = PolylineShape(points)
          def setAttribute[T : Manifest](name:String, shape:Shape) = {
            Siigna.get(name) match {
              case s : Some[T] => shape.addAttribute(name, s.get)
              case None => shape// Option isn't set. Do nothing
            }
          }
          //val polyline = setAttribute[Color](Siigna.activeColor,setAttribute[Double]("LineWeight", plShape))
          val polyline = plShape
          Create(polyline)
        }
        //The module closes - even if no polyline was drawn.
      startPoint = None
      points = List()
      End
      }
      case x => {
        Start('Input,"com.siigna.module.base.create",1)
      }
    })

}
/*

  var points = List[Vector2D]()

  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, 0.30f)

  def stateMap = DirectedGraph(

    'StartCategory    ->   'Message  ->    'SetPoint,
    'StartCategory    ->   'KeyDown  ->    'End
  )

  def stateMachine = Map(
    'StartCategory -> ((events : List[Event]) => {
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => {
          'End
        }
        case _ => Module('Point)
      }
    }),
  'SetPoint -> ((events : List[Event]) => {

    //send a guide drawing the area dynamically as points are added:
    def getPointGuide = (p : Vector2D) => {

      if(!points.isEmpty) {
        val closedPolyline = points.reverse :+ p
        val areaGuide = closedPolyline.reverse :+ p
          if(Siigna.get("activeColor").isDefined)) PolylineShape((areaGuide)).setAttributes("raster" -> Siigna("activeColor"), "StrokeWidth" -> 0.0)
          else PolylineShape((areaGuide)).setAttributes("raster" -> anthracite, "StrokeWidth" -> 0.0)
    }
      else PolylineShape(Vector2D(0,0),Vector2D(0,0))
    }
    events match {
      // Exit strategy
      case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

      case Message(p : Vector2D) :: tail => {
        // Save the point
        points = points :+ p
        // Define shape if there is enough points
        Module('Point)
        Controller ! Message(PointGuide(getPointGuide))
      }

      // Match on everything else
      case _ => {
        Module('Point)
        Controller ! Message(PointGuide(getPointGuide))
      }
    }
  }),
    'End -> ((events : List[Event]) => {
      //add a line segment from the last to the first point in the list to close the fill area
      if (points.size > 2 && Siigna.get("activeColor").isDefined)
        CreateCategory(PolylineShape(points :+ points(0)).setAttributes("raster" -> Siigna("activeColor"), "StrokeWidth" -> 0.0))
      else CreateCategory(PolylineShape(points :+ points(0)).setAttributes("raster" -> anthracite, "StrokeWidth" -> 0.0))

      //Clear the variables
      points = List[Vector2D]()
      com.siigna.module.base.ModuleInit.previousModule = Some('Area)

    })
  )
}*/