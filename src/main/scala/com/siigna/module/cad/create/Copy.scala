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
import module.Tooltip

class Copy extends Module {

  var endPoint : Option[Vector2D] = None
  var multiActive = false
  var startPoint : Option[Vector2D] = None
  var transformation = TransformationMatrix()

  /**
   * Transform the shapes with the given transformation
   * @param t The transformation
   * @return The shapes transformed
   */
  def transform(t : TransformationMatrix) = {
    Drawing.selection.shapes.map(_._2.transform(t))
  }

  val stateMap: StateMap = Map(

    'Start -> {

      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End

      case End(p : Vector2D) :: tail => {

        if(!startPoint.isDefined && !Drawing.selection.isEmpty) {
          startPoint = Some(p)
          Siigna display "set destination"
          Tooltip.blockUpdate(3500)
          val vector2DGuide = Vector2DGuide((v : Vector2D) => {
            transform(TransformationMatrix(v - startPoint.get, 1))
          })

          val inputRequest = InputRequest(5,startPoint,vector2DGuide)
          Start('cad, "create.Input", inputRequest)
        } else if (startPoint.isDefined){
          endPoint = Some(p)
          transformation = TransformationMatrix(p - startPoint.get, 1)
          Siigna display "type number of copies or click for one"
          Tooltip.blockUpdate(3500)
          multiActive = true

          val doubleGuide = DoubleGuide((r: Double) => transform(transformation))
          val vector2DGuide = Vector2DGuide((v: Vector2D) => transform(transformation))
          val inputRequest = InputRequest(13, None,vector2DGuide,doubleGuide)
          Start('cad, "create.Input", inputRequest)
        }
      }

      case End(f : Double) :: tail => {
        if (multiActive && endPoint.isDefined){
          var g: Double = f
          if (g == 0) g = 1
          if (g < 0) g = math.abs(g)
          val shapes = for (i <- 1 to g.toInt) yield {
             transform(
               TransformationMatrix(
                 Vector2D(
                   (endPoint.get.x - startPoint.get.x) * i,
                   (endPoint.get.y - startPoint.get.y) * i
                 ),
                 1
               )
             )
          }
          Create(shapes.flatten)
        }
        End
      }

      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => {
        if (multiActive) {
          transformation = TransformationMatrix(endPoint.get - startPoint.get, 1)
          Create(transform(transformation))
          val module = Module('base, "Menu")
          End(module)
        } else End
      }

      case End(KeyDown(key,modifier)) :: tail => {
        if (key == Key.escape) {
          if (multiActive) {

            transformation = TransformationMatrix(endPoint.get - startPoint.get, 1)
            Create(transform(transformation))
          }
          End('base, "Menu")
        }
      }

      case _ => {
        if (Drawing.selection.isDefined) {
          if (multiActive) {
            Create(transform(transformation))
            End
          } else {
            Tooltip.updateTooltip("Copy tool active")
            Siigna display "set origin of copy"
            Tooltip.blockUpdate(3500)
            Start('cad,"create.Input",InputRequest(6,None))
          }
        } else {
          Tooltip.updateTooltip("Copy tool active")
          Siigna display "Select objects to copy"
          Tooltip.blockUpdate(3500)
          Start('cad, "Selection")
        }
      }
    }

  )
}