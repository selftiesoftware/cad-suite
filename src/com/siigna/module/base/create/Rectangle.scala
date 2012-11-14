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

class Rectangle extends Module {

  var points = List[Vector2D]()
  val stateMap: StateMap = Map(

    'Start -> {
      case End(v : Vector2D) :: tail => {
        //use the first point
        if (points.length == 0){
          points = points :+ v

          val guide = PointGuide(v, (v : Vector2D) => {
            (Array(PolylineShape(Rectangle2D(points(0), v))))
          },1) //1 : Input type = InputTwoValues

          Start('Point,"com.siigna.module.base.create", guide)
          }
        //use second point
        else if (points.length == 1) {
          points = points :+ v
          //create the rectangle
          Create(PolylineShape(Rectangle2D(points(0), points(1))))
          End
        }
      }

      case End("no point returned") :: tail => {
          if (points.length == 0) {
            Start('Point,"com.siigna.module.base.create",1)
          } else {
            val guide = PointGuide(points.head, (v : Vector2D) => {
              (Array(PolylineShape(Rectangle2D(points(0), v))))
            },1)//1 : Input type = InputTwoValues
            Start('Point,"com.siigna.module.base.create", guide)
          } }

      //If End with no point: End module without drawing anything.
      case End :: tail => End
     //get the first point
      case _ => {
        if (points.length == 0) {
          Start('Point,"com.siigna.module.base.create",1)
        } else {
          val guide = PointGuide(points.head, (v : Vector2D) => {
            (Array(PolylineShape(Rectangle2D(points(0), v))))
          },1)//1 : Input type = InputTwoValues
          Start('Point,"com.siigna.module.base.create", guide)
        } }
    }
  )
}