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
import com.siigna.module.porter.DXF.DXFExporter
import module.Tooltip
import com.siigna.Drawing
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection

class Copy extends Module {

  var endPoint : Option[Vector2D] = None
  var multiActive = false
  var multiFirst = true
  var startPoint : Option[Vector2D] = None
  var transformation = TransformationMatrix()
  val shapes = Drawing.selection.shapes

  /**
   * Transform the shapes with the given transformation
   * @param t The transformation
   * @return The shapes transformed
   */
  def transform(t : TransformationMatrix) = {
    shapes.map(_._2.transform(t))
  }

  //a function used to put a selection of shapes on the clipboard. Used for making icons and such.
  def shapesToClipboard(shapes : Map[Int,Shape]) {
    val clip = Toolkit.getDefaultToolkit.getSystemClipboard
    val shapesList = List(shapes.map(_._2))
    val s : StringSelection = new StringSelection(shapesList.toString())
    clip.setContents(s,s)
  }

  val stateMap: StateMap = Map(

    'Start -> {

      //exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | KeyDown(Key.Enter, _) | End(KeyDown(Key.enter, _)) |
            MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => {
        if (multiFirst == true && startPoint.isDefined && endPoint.isDefined) {
          transformation = TransformationMatrix(endPoint.get - startPoint.get, 1)
          Create(transform(transformation))
        }
        Siigna display ""
        End
      }



      case End(p : Vector2D) :: tail => {

        if(!startPoint.isDefined && !Drawing.selection.isEmpty) {
          startPoint = Some(p)

          Siigna display "set destination"
          Tooltip.blockUpdate(3500)
          val vector2DGuide = DynamicDrawFromVector2D((v : Vector2D) => {
            transform(TransformationMatrix(v - startPoint.get, 1))
          })

          val inputRequest = InputRequest(5,startPoint,vector2DGuide)
          Start('cad, "create.Input", inputRequest)
        } else if (startPoint.isDefined & multiActive == false){
          endPoint = Some(p)
          Siigna display "click to set another copy or type number of copies. Enter, escape or right mouse to end."
          Tooltip.blockUpdate(3500)
          multiActive = true
          var t: Traversable[Shape] = Traversable()
          val doubleGuide = DynamicDrawFromDouble((r: Double) => transform(TransformationMatrix(endPoint.get-startPoint.get)))
          val vector2DGuide = DynamicDrawFromVector2D((v: Vector2D) => transform(TransformationMatrix(endPoint.get - startPoint.get,1)) ++ transform(TransformationMatrix(v-startPoint.get,1)))
          val inputRequest = InputRequest(9, None,vector2DGuide,doubleGuide)
          Start('cad, "create.Input", inputRequest)
        } else if (multiActive == true) {
          if (multiFirst == true) {
            transformation = TransformationMatrix(endPoint.get - startPoint.get, 1)
            Create(transform(transformation))
          }
          multiFirst = false
          //First copy set with click. user might want to set more...
          transformation = TransformationMatrix(p - startPoint.get, 1)
          Create(transform(transformation))
          Siigna display "click to set another copy. Enter, escape or right mouse to end."
          Tooltip.blockUpdate(3500)
          val vector2DGuide = DynamicDrawFromVector2D((v: Vector2D) => transform(TransformationMatrix(v-startPoint.get,1)))
          val inputRequest = InputRequest(1 , None,vector2DGuide)
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

      case _ => {
        if (Drawing.selection.isDefined) {

          //DEFAULT: add selection to clipboard - use this to get DXF data on the clipboard.
          DXFExporter.toDXFtoClipboard(shapes)

          //use this for saving shapes when drawing new / revised Siigna tool icons
          //shapesToClipboard(shapes)

          //change cursor to crosshair
          Siigna.setCursor(Cursors.crosshair)
            Tooltip.updateTooltip(List("Copy tool active","",""))
            Siigna display "set origin of copy"
            Tooltip.blockUpdate(3500)
            Start('cad,"create.Input",InputRequest(6,None))
        } else {
          if (Drawing.size > 0) {
            Tooltip.updateTooltip(List("Copy tool active","",""))
            Siigna display "Select objects to copy"
            Tooltip.blockUpdate(3500)
            Start('cad, "Selection")
          } else {
            Siigna display "No shapes to copy"
            Tooltip.blockUpdate(3500)
            End
          }
        }
      }
    }

  )
}