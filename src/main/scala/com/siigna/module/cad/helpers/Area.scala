/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.helpers

import com.siigna._
import com.siigna.module.Module
import com.siigna.module.cad.create._

import java.awt.Color
import com.siigna.app.model.shape.PolylineShape.PolylineShapeClosed

/**
 * A module that measures and displays an area.
 */

//TODO: add ability to measure the area of one or more selected, closed polylines.
class Area extends Module {

  //color for the dynamically drawn area
  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, 0.30f)

  //a function to calculate an area defined by points.
  def area(points : List[Vector2D]) = {

    var area : Double = 0
    var i : Int = 0

    //TODO: Add the first element to the end to close the polygon

    while (i < points.length - 1) {
      var pointX1 = points(i).x
      var pointY1 = points(i).y

      var pointX2 = points(i+1).x
      var pointY2 = points(i+1).y

      area += pointX1 * pointY2 - pointX2 * pointY1
      i += 1
    }
    area = scala.math.abs(area * 0.5)
    area.toInt
  }

  //change display different units based on area size
  def units(a : Int) = {
    if(a < 100) a+ " mm2"
    else if(a >= 100 && a < 500000) "%.2f".format(a/100.toDouble)+" cm2"
    else "%.2f".format(a/1000000.toDouble) +" m2"
  }


  var color = new Color(1.00f, 0.75f, 0.30f, 0.60f)
  val lineWidth = 4.0
  private var points   = List[Vector2D]()
  var startPoint: Option[Vector2D] = None

  val stateMap: StateMap = Map(
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => {
        if (points.length > 2) {
          //a function to calculate the center of the area
          Siigna.display("Area: "+units(area(points :+ points(0))))
          //TODO: calculate the weighted center of the polygon and place a TextShape with the area there.
          //Create(TextShape(units(area(points :+ points(0))),position(points) ,3 * Siigna.paperScale))
          points = List()
        }
        End
      }

      case End(p : Vector2D) :: tail => {
        //if the point module returns with END and a point, a new point is received.
        //points = points :+ p
        if (points.isEmpty){
          //If the start point is not yet set, then the first segment is being drawn, which means a guide can be made.
          points = points :+ p
          val vector2DGuide = Vector2DGuideNew((v: Vector2D) => Traversable(PolylineShape(points(0),v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequestNew(7,Some(p),vector2DGuide)
          Start('cad, "create.InputNew", inputRequest)
        } else {
          //If the start point is set, the first segment is made and points should be added.
          points = points :+ p
          //create a guide which adds the mouse position.
          val vector2DGuide = Vector2DGuideNew((v: Vector2D) => {
            var closedPl = points :+ v
            closedPl = closedPl :+ points(0)
            var pts = points :+ v.transform(View.deviceTransformation)
            pts = pts :+ points(0)
            Siigna.display("Area: "+units(area(pts)))
            pts = List()
            Traversable(PolylineShape(closedPl).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth))
          })
          //a message which display the current area, to be send to the Input module
          /*val vector2DMessageGuide = Vector2DMessageGuideNew((v: Vector2D) =>  {
            var pts = points :+ v.transform(View.deviceTransformation)
            pts = pts :+ points(0)
            Siigna.display("Area: "+units(area(pts)))
            pts = List()
          })*/
          val inputRequest = InputRequestNew(7,Some(points.last),vector2DGuide)
          Start('cad, "create.InputNew", inputRequest)
          //Start('cad, "create.Input")
          //val guide = PointPointGuide(v, (v : Vector2D) => {
          //  Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth))
          //},112)//1 : Input type = InputTwoValues
        }
      }

      //If input module does not return any input:
      case End("no point returned") :: tail => {
        //If there only is the start point:
        if (points.length == 1){
          //If the start point is not yet set, then the first segment is being drawn, which means a guide can be made.
          startPoint = Some(points.last)
          val vector2DGuide = Vector2DGuideNew((v: Vector2D) => Traversable(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequestNew(7,startPoint,vector2DGuide)
          Start('cad, "create.InputNew", inputRequest)
        } else {
          val vector2DGuide = Vector2DGuideNew((v: Vector2D) => {
            var pts = points :+ v.transform(View.deviceTransformation)
            pts = pts :+ points(0)
            Siigna.display("Area: "+units(area(pts)))
            pts = List()
            Traversable(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth))
          })
          /*val vector2DMessageGuide = Vector2DMessageGuideNew((v: Vector2D) => {
            var pts = points :+ v.transform(View.deviceTransformation)
            pts = pts :+ points(0)
            Siigna.display("Area: "+units(area(pts)))
            pts = List()
          })*/
          val inputRequest = InputRequestNew(7,Some(points.last),vector2DGuide)
          Start('cad, "create.InputNew", inputRequest)
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
            val vector2DGuide = Vector2DGuideNew((v: Vector2D) => Traversable(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
            val inputRequest = InputRequestNew(7,Some(points.last),vector2DGuide)
            Start('cad, "create.InputNew", inputRequest)
          } else {
            //If not, point is started without guide.
            Start('cad, "create.InputNew",InputRequestNew(6,None))
          }
        }
      }

      case End :: tail => {
        //If there are two or more points in the polyline, it can be saved to the Siigna universe.
        if (points.length > 2) {
          Siigna.display("Area: "+units(area(points :+ points(0))))
        }
        //The module closes - even if no polyline was drawn.
        startPoint = None
        points = List()
        End
      }
      case x => {
        Start('cad, "create.InputNew", InputRequestNew(6,None))
      }
    }
  )
}