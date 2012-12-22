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

package com.siigna.module.base.helpers

import com.siigna._
import com.siigna.module.Module
import app.controller.Controller
import com.siigna.module.base.create._

import java.awt.Color
import scala.Array._

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

      //area += (x.Items.Item(i) * y.Items.Item(i + 1) - x.Items.Item(i + 1) * y.Items.Item(i))
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
  var savedArea : Double = 0

  val stateMap: StateMap = Map(
    'Start -> {
      case End(p : Vector2D) :: tail => {
        //if the point module returns with END and a point, a new point is received.
        points = points :+ p
        if (startPoint.isEmpty){
          //If the start point is not yet set, then the first segment is being drawn, which means a guide can be made.
          startPoint = Some(p)
          val vector2DGuide = Vector2DGuide((v: Vector2D) => Traversable(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,Some(p),None,None,Some(112))
          Start('Input, "com.siigna.module.base.create",inputRequest)
        } else {
          //If the start point is set, the first segment is made and points should be added.
          points :+ p
          val vector2DGuide = Vector2DGuide((v: Vector2D) => Traversable(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val vector2DMessageGuide = Vector2DMessageGuide((v: Vector2D) => Siigna.display("Area: "+units(area(((points.reverse :+ v).reverse) :+ v))))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,Some(vector2DMessageGuide),None,None,Some(points.last),None,None,Some(112))
          Start('Input, "com.siigna.module.base.create",inputRequest)
          //Start('Input,"com.siigna.module.base.create")
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
          val vector2DGuide = Vector2DGuide((v: Vector2D) => Traversable(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,startPoint,None,None,Some(112))
          Start('Input, "com.siigna.module.base.create",inputRequest)
        } else {
          val vector2DGuide = Vector2DGuide((v: Vector2D) => Traversable(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,Some(points.last),None,None,Some(112))
          Start('Input, "com.siigna.module.base.create",inputRequest)
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
            val vector2DGuide = Vector2DGuide((v: Vector2D) => Traversable(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
            val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,Some(points.last),None,None,Some(112))
            Start('Input, "com.siigna.module.base.create",inputRequest)
          } else {
            //If not, point is started without guide.
            Start('Input,"com.siigna.module.base.create")
          }
        }
      }

      case End(MouseDown(p, MouseButtonRight, _)) :: tail => {
        if (points.length > 2) {
          Siigna.display("Area: "+units(area(points :+ points(0))))
          Create(TextShape(units(area(points :+ points(0))),Vector2D(0,0),3 * Siigna.paperScale))
        }
        End
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
        Start('Input,"com.siigna.module.base.create",111)
      }
    }
  )
}