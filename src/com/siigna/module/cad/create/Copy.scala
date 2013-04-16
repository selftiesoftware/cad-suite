/*
 * Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
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

class Copy extends Module {

  var endPoint : Option[Vector2D] = None
  var multiActive = false
  var startPoint : Option[Vector2D] = None
  var transformation = TransformationMatrix()

  // The selection
  def selection = Drawing.selection

  // Transforms the selection and returns the resulting shapes
  def transform(t : TransformationMatrix) = selection.transform(t).shapes.values

  val stateMap: StateMap = Map(

    'Start -> {
      case End(p : Vector2D) :: tail => {
        if(!startPoint.isDefined && !Drawing.selection.isEmpty) {
          startPoint = Some(p)
          Siigna display "set destination"
          val vector2DGuide = Vector2DGuide((v : Vector2D) => transform(TransformationMatrix(v - startPoint.get, 1)))
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,startPoint,None,None,Some(1))
          Start('cad, "create.Input", inputRequest)
        }
        else if(startPoint.isDefined){
          endPoint = Some(p)
          transformation = TransformationMatrix((p - startPoint.get), 1)
          Siigna display "type number of copies or click for one"
          multiActive = true
          val doubleGuide = DoubleGuide((r: Double) => transform(transformation))
          val vector2DGuide = Vector2DGuide((v: Vector2D) => transform(transformation))
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
            Create(transform(TransformationMatrix(Vector2D((endPoint.get.x - startPoint.get.x) * i, (endPoint.get.y - startPoint.get.y) * i), 1)))
          }
        }
        End
      }

      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => {
        if (multiActive == true) {
          transformation = TransformationMatrix((endPoint.get - startPoint.get), 1)
          Create(transform(transformation))
          val module = Module('base, "Menu")
          End(module)
        } else End
      }

      case End(KeyDown(key,modifier)) :: tail => {
        if (key == Key.escape) {
          if (multiActive == true) {
            transformation = TransformationMatrix((endPoint.get - startPoint.get), 1)
            Create(transform(transformation))
          }
          End('base, "Menu")
        }
      }


      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      case _ => {
        if (Drawing.selection.isDefined) {
          if(multiActive == true) {
            Create(transform(transformation))
            End
          }
          else {
            Siigna display "set origin of copy"
            Start('cad,"create.Input",1)
          }
        } else {
          Siigna display "nothing selected"
          End
        }
      }
    }
  )
}