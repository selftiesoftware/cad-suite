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

package com.siigna.module.cad.create

import com.siigna._
//import app.model.shape.RectangleShape
import com.siigna.Siigna

class Rectangle extends Module {
  var color = Siigna("activeColor")
  val stroke = Siigna("activeLineWidth")
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
          val vector2DGuide = Vector2DGuideNew((v: Vector2D) => Traversable(PolylineShape(Rectangle2D(points(0), v)).addAttributes("Color" -> color , "StrokeWidth" -> stroke)))
          val inputRequest = InputRequestNew(7,Some(points(0)),vector2DGuide)
          Start('cad,"create.InputNew", inputRequest)
          }
        //use second point
        else if (points.length == 1) {
          points = points :+ v
          val width = (points(1).x - (points(0).x))
          val height = (points(1).y - (points(0).y))
          val center = Vector2D((points(0).x + width/2),(points(0).y + height/2))
          //create the rectangle
          Create(RectangleShape(center,width,height,0.0,Attributes("Color" -> color , "StrokeWidth" -> stroke)))
          points = List()
          End
        }
      }

      case End("no point returned") :: tail => {
        if (points.length == 0) {
          Start('cad, "create.InputNew", InputRequestNew(6,None))
        } else {
          val vector2DGuide = Vector2DGuideNew((v: Vector2D) => Traversable(PolylineShape(Rectangle2D(points(0), v)).addAttributes("Color" -> color , "StrokeWidth" -> stroke)))
          val inputRequest = InputRequestNew(7,Some(points(0)),vector2DGuide)
          Start('cad,"create.InputNew", inputRequest)
        }
      }

      //If End with no point: End module without drawing anything.
      case End :: tail => End
     //get the first point
      case _ => {


        //Create(RectangleShape(Vector2D(40,40),30,30,45,Attributes()))
        if (points.length == 0) {
          Start('cad, "create.InputNew", InputRequestNew(6,None))
        } else {
          val vector2DGuide = Vector2DGuideNew((v: Vector2D) => Traversable(PolylineShape(Rectangle2D(points(0), v)).addAttributes("Color" -> color , "StrokeWidth" -> stroke)))
          val inputRequest = InputRequestNew(7,Some(points(0)),vector2DGuide)
          Start('cad,"create.InputNew", inputRequest)
        }
      }
    }
  )
}