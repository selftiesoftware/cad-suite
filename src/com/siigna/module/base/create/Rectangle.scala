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
import com.siigna.Siigna

class Rectangle extends Module {
  var color = Siigna.color("activeColor")
  val lineWidth = Siigna.double("activeLineWidth")
  var points = List[Vector2D]()
  val stateMap: StateMap = Map(

    'Start -> {
      //exit mechanisms
      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End
      case End(KeyDown(Key.escape,modifier)) :: tail => End
      case MouseDown(p,MouseButtonRight,modifier) :: tail => End
      case KeyDown(Key.escape,modifier) :: tail => End

      case End(v : Vector2D) :: tail => {
        //use the first point
        if (points.length == 0){
          points = points :+ v
          val vector2DGuide = Vector2DGuide((v: Vector2D) => Traversable(PolylineShape(Rectangle2D(points(0), v)).addAttributes("Color" -> color , "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,Some(points(0)),None,None,Some(112))
          Start('Input, "com.siigna.module.base.create",inputRequest)
          }
        //use second point
        else if (points.length == 1) {
          points = points :+ v
          //create the rectangle
          Create(PolylineShape(Rectangle2D(points(0), points(1))).addAttributes("Color" -> color , "StrokeWidth" -> lineWidth))
          End
        }
      }

      case End("no point returned") :: tail => {
        if (points.length == 0) {
          Start('Point,"com.siigna.module.base.create",111)
        } else {
          val vector2DGuide = Vector2DGuide((v: Vector2D) => Traversable(PolylineShape(Rectangle2D(points(0), v)).addAttributes("Color" -> color , "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,Some(points(0)),None,None,Some(112))
          Start('Input, "com.siigna.module.base.create",inputRequest)
        }
      }

      //If End with no point: End module without drawing anything.
      case End :: tail => End
     //get the first point
      case _ => {
        if (points.length == 0) {
          Start('Input,"com.siigna.module.base.create",111)
        } else {
          val vector2DGuide = Vector2DGuide((v: Vector2D) => Traversable(PolylineShape(Rectangle2D(points(0), v)).addAttributes("Color" -> color , "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,Some(points(0)),None,None,Some(112))
          Start('Input, "com.siigna.module.base.create",inputRequest)
        } 
      }
    }
  )
}