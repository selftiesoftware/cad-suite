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
package com.siigna.module.cad.edit

import com.siigna._
import app.Siigna
import app.model.shape.PolylineShape.PolylineShapeOpen
import module.cad.create.InputRequest
import module.cad.joinMethods
import module.Tooltip

class Join extends Module{

  /**
   * a function to join the lines and polylines which can be joined within a list of shapes.
   * input: a list of shapes
   * return: the joined shapes
   * old shapes are deleted.
   */

  //the attributes to give to the joined shape
  private var attr = Attributes()


  //STRATEGY:
  //1) if sel.size == 2
  //2) get endPoints
  //3) if coinsiding ends: Create(newS = s1+s2)
  //4)del s1 and s2
  //5) select newS
  //6) Start(Select)

  def m = mousePosition.transform(View.deviceTransformation)

  def nearestShape : Option[(Int, Shape)] = {
    val drawing = Drawing(m)
    if (!drawing.isEmpty) {
      Some(drawing.reduceLeft((a, b) => if (a._2.distanceTo(m) < b._2.distanceTo(m)) a else b))
    } else None
  }

  def shapeWithinSelectionDistance: Boolean = {
    if (nearestShape.isDefined) {
      if (nearestShape.get._2.distanceTo(m) < Siigna.selectionDistance) true
      else false
    } else {
      false
    }
  }

  var selection = Selection()

  var selectIDs : List[Int] = List()

  val stateMap: StateMap = Map(

    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => {
        Deselect()
        End
      }
      case MouseDown(p, MouseButtonRight, _) :: tail => {
        Deselect()
        End
      }
      case End(KeyDown(Key.Esc, _)) :: tail => {
        Deselect()
        End
      }
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => {
        Deselect()
        End
      }

      //look for a mouse point indicating that another shape is chosen for join evaluation
      case MouseDown(p, MouseButtonLeft, _) :: tail => {
        if (!shapeWithinSelectionDistance) Deselect()
        else {
          if(nearestShape.isDefined) {
            val (id, shape) = nearestShape.get
            selectIDs = selectIDs :+ id
            //Deselect()
            Select(selectIDs)
          }
        }
        if(Drawing.selection.size >= 2) 'Join else 'Start
      }

      //else do nothing
      case _ => {
        Tooltip.updateTooltip(List("click to join shapes. Right click or ESC to exit"))

        if(Drawing.selection.size >= 2) {
          'Join
        }
      }
    },

    'Join -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case _ => {
        val selectionShapes = Drawing.selection.shapes.map(s => s._2)

        //joining of two shapes
        if(selectionShapes.size == 2) {
          selection = Drawing.selection //save the selection so that the original shapes can be deleted

          Siigna display "joining shapes"

          //2) get endPoints
          val s1 = selectionShapes.head
          val s2 = selectionShapes.last

          attr = s1.attributes
          val joined = joinMethods.joinTwoShapes(s1,s2,attr)

          if(joined.isDefined) {
            Delete(selection)
            Create(joined.get)
            selectIDs = List()
            selection = Selection()
            Deselect()
            'Start
          }
        }

        else if (selectionShapes.size > 2) {

          selection = Drawing.selection //save the selection so that the original shapes can be deleted

          //join lines
          val shapes =joinMethods.joinMultiple(Drawing.selection.shapes)

          if(!shapes.isEmpty) {
            Delete(selection)
            Create(shapes)
          }

          selectIDs = List()
          selection = Selection()
          Deselect()
          End

        } else {
          Siigna display "Select two objects to join"
          Start('cad, "Selection")
        }
        //End the module
        //End
      }
    }
  )
}
