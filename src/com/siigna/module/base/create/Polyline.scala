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

class Polyline extends Module {

  println(Siigna.color("activeColor"))
  println(Siigna.double("activeLineWidth"))

  var color = Siigna.color("activeColor")
  val lineWidth = Siigna.double("activeLineWidth")
  var startPoint: Option[Vector2D] = None
  private var points   = List[Vector2D]()

  val stateMap: StateMap = Map(
    'Start -> {
      case End(v : Vector2D) :: tail => {
        //if the point module returns with END and a point, a new point is received.
        points = points :+ v
        if (startPoint.isEmpty){
          //If the start point is not yet set, then the first segment is being drawn, which means a guide can be made.
          startPoint = Some(v)

          val guide = PointPointGuide(v, (v : Vector2D) => {
            (Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          },112)//1 : Input type = InputTwoValues

          Start('Input,"com.siigna.module.base.create", guide)
        } else {
          //If the start point is set, the first segment is made and points should be added.
          points :+ v
          //val guide : Guide = Guide((v : Vector2D) => {
          //  Array(PolylineShape(points :+ v))
          //})
          val guide = PointPointGuide(v, (v : Vector2D) => {
            Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth))
          },112)//1 : Input type = InputTwoValues
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
            Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth))
          },112)//1 : Input type = InputTwoValues

          Start('Input,"com.siigna.module.base.create", guide)
        } else {

          val guide = PointPointGuide(points.last, (v : Vector2D) => {
            Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth))
          },112)//1 : Input type = InputTwoValues
          Start('Input,"com.siigna.module.base.create", guide)
        }
      }

      //If point module returns a key-pres at the event when it ends:
      case End(k : KeyDown) :: tail => {
        println("Key in polyline: " + k)
        // If the key is backspace without modification (shift etc), the last bit of line is deleted:
        if (k == KeyDown(Key.Backspace,ModifierKeys(false,false,false))) {
          if (points.length > 1) {
            println("Points - wil be shortened: " + points)
            points = points.dropRight(1)
          }
          //And if there is a start point, a new guide is returned
          if (startPoint.isDefined) {
            val guide : PointPointGuide = PointPointGuide(points.last, (v : Vector2D) => {
              Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth))
            },112) //1 : Input type = InputTwoValues
            Start('Input,"com.siigna.module.base.create", guide)
          } else {
            //If not, point is started without guide.
            Start('Input,"com.siigna.module.base.create")
          }
        }}

      case End(MouseDown(p, MouseButtonRight, _)) :: tail => {
        val polyline = PolylineShape(points).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)
        Create(polyline)
        End
      }

      case End :: tail => {
        //If there are two or more points in the polyline, it can be saved to the Siigna universe.
        if (points.length > 1) {
          val polyline = PolylineShape(points).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)
          Create(polyline)
        }
        //The module closes - even if no polyline was drawn.
        startPoint = None
        points = List()
        End
      }
      case x => {
        Start('Input,"com.siigna.module.base.create",111)
      }
    })

}