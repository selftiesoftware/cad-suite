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

package com.siigna.module.cad.modify

import com.siigna._
import app.Siigna
import com.siigna.module.ModuleInit
import com.siigna.module.cad.create._
import module.cad.create.{InputRequestNew, Vector2DGuideNew}

class TrimByBox extends Module {

  var color = Siigna("activeColor")
  val lineWidth = Siigna("activeLineWidth")
  var startPoint: Option[Vector2D] = None
  private var points   = List[Vector2D]()

  val stateMap: StateMap = Map(
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => {
        if (points.length > 1) {
          //HER SKAL DER TRIMMES
        }
        End
      }

      case End(v : Vector2D) :: tail => {
        //if the point module returns with END and a point, a new point is received.
        points = points :+ v
        if (startPoint.isEmpty){
          //If the start point is not yet set, then the first segment is being drawn, which means a guide can be made.
          startPoint = Some(v)
          val guide = Vector2DGuideNew((v : Vector2D) => Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequestNew(1,startPoint,guide)
          Start('cad, "create.InputNew", inputRequest)
        } else {

          //If the start point is set, the first segment is made and points should be added.
          points :+ v
          val guide = Vector2DGuideNew ((v : Vector2D) => Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequestNew(1,Some(points.last),guide)
          Start('cad, "create.InputNew", inputRequest)
        }
      }

      //If input module does not return any input:
      case End("no point returned") :: tail => {
        //If there only is the start point:
        if (points.length == 1){
          //If the start point is not yet set, then the first segment is being drawn, which means a guide can be made.
          startPoint = Some(points.last)
          val guide = Vector2DGuideNew((v : Vector2D) => Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequestNew(1,startPoint,guide)
          Start('cad, "create.InputNew", inputRequest)
        } else {
          val guide = Vector2DGuideNew((v : Vector2D) => Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
          val inputRequest = InputRequestNew(1,Some(points.last),guide)
          Start('cad, "create.InputNew", inputRequest)
        }
      }

      //If point module returns a key-pres at the event when it ends:
      case End(k : KeyDown) :: tail => {
        // If the key is backspace without modification (shift etc), the last bit of line is deleted:
        if (k == KeyDown(Key.Backspace,ModifierKeys(false,false,false))) {
          if (points.length > 1) {
            println("Points - will be shortened: " + points)
            points = points.dropRight(1)
          }
          //And if there is a start point, a new guide is returned
          if (startPoint.isDefined) {

            val guide = Vector2DGuideNew((v : Vector2D) => Array(PolylineShape(points :+ v).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)))
            val inputRequest = InputRequestNew(1, Some(points.last),guide)
            Start('cad, "create.InputNew", inputRequest)
          } else {
            //If not, point is started without guide.
            Start('cad, "create.InputNew",InputRequestNew(1,None))
          }
        }}

      case End :: tail => {
        //If there are two or more points in the polyline, it can be saved to the Siigna universe.
        if (points.length > 1) {
          val polyline = PolylineShape(points).addAttributes("Color" -> color, "StrokeWidth" -> lineWidth)
          Create(polyline)
          points = List[Vector2D]()
        }
        //The module closes - even if no polyline was drawn.
        startPoint = None
        points = List[Vector2D]()
        End
      }
      case x => {
        points = List[Vector2D]()
        Start('cad, "create.InputNew", InputRequestNew(1,None))
      }
    }
  )
}