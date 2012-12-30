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

package com.siigna.module.cad.create

import com.siigna._
import app.Siigna
import module.ModuleInit

class Copy extends Module {

  var endPoint : Option[Vector2D] = None
  var multiActive = false
  var startPoint : Option[Vector2D] = None
  var shapes : Option[Selection] = None
  var transformation : Option[TransformationMatrix] = None

  val stateMap: StateMap = Map(

    'Start -> {
      case End(p : Vector2D) :: tail => {
        if(!startPoint.isDefined && !Drawing.selection.get.isEmpty) {
          shapes = Some(Drawing.selection.get)
          startPoint = Some(p)
          Siigna display "set destination"
          val vector2DGuide = Vector2DGuide((v : Vector2D) => Drawing.selection.get.apply(TransformationMatrix(v - startPoint.get, 1)))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,startPoint,None,None,Some(1))
          Start('cad, "create.Input", inputRequest)
        }
        else if(startPoint.isDefined){
          endPoint = Some(p)
          transformation = Some(TransformationMatrix((p - startPoint.get), 1))
          Siigna display "type number of copies or click for one"
          multiActive = true
          val doubleGuide = DoubleGuide((r: Double) => shapes.get.apply(transformation.get))
          val vector2DGuide = Vector2DGuide((v: Vector2D) => shapes.get.apply(transformation.get))
          val inputRequest = InputRequest(Some(vector2DGuide),Some(doubleGuide),None,None,None,None,startPoint,None,None,Some(17))
          Start('cad, "create.Input", inputRequest)
        }
      }

      case End(f : Double) :: tail => {
        if (multiActive == true && endPoint.isDefined){
          var g: Double = f
          if (g == 0) g = 1
          if (g < 0) g = math.abs(g)
          for (i <- 1 to g.toInt) {
            Create(shapes.get.apply(TransformationMatrix(Vector2D((endPoint.get.x - startPoint.get.x) * i, (endPoint.get.y - startPoint.get.y) * i), 1)))
          }
        }
        End
      }

      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => {
        if (multiActive == true) {
          transformation = Some(TransformationMatrix((endPoint.get - startPoint.get), 1))
          Create(shapes.get.apply(transformation.get))
          val module = Module('base, "Menu")
          End(module)
        } else End
      }

      case End(KeyDown(key,modifier)) :: tail => {
        if (key == Key.escape) {
          println("jhgyu")
          if (multiActive == true) {
            transformation = Some(TransformationMatrix((endPoint.get - startPoint.get), 1))
            Create(shapes.get.apply(transformation.get))
          }
          End('base, "Menu")
        }
      }


      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      case _ => {
        //Should be done differently, but this is how I can reach this (usableSelectionExists) function just quickly...
        val l = new ModuleInit

        if (l.usableSelectionExists) {
          if(multiActive == true) {
            Create(shapes.get.apply(transformation.get))
            End
          }
          else {
            Siigna display "set origin of copy"
            Start('base,"create.Input",1)
          }
        } else {
          Siigna display "nothing selected"
          End
        }
      }
    }
  )
}