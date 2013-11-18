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

/*
A module to allow users to take properties (color and lineweight) from a shape
and transfer it to other shapes.
 */

class MatchProperties extends Module {

  def m = mousePosition.transform(View.deviceTransformation)
  //val selection = Drawing.selection
  var properties : Option[Attributes] = None

  def nearestShape : Option[(Int, Shape)] = {
    val drawing = Drawing(m)
    if (!drawing.isEmpty) {
      Some(drawing.reduceLeft((a, b) => if (a._2.distanceTo(m) < b._2.distanceTo(m)) a else b))
    } else None
  }

  def lessThanSelDist : Boolean = {
    if (nearestShape.isDefined) {
      if (nearestShape.get._2.distanceTo(m) < Siigna.selectionDistance) true
      else false
    } else {
      false
    }
  }

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

      case MouseUp(p, MouseButtonLeft, _) :: tail => {
        //add the properties to the selected shape
        if(Drawing.selection.isEmpty && nearestShape.isDefined && lessThanSelDist && !properties.isDefined) {
          properties = Some(nearestShape.get._2.attributes)
          println("#prop in start; "+properties)
        }
      }

      case e => {
        Siigna.setCursor(Cursors.invisible)

        //if there is already a selection, goto the next state
        if (!properties.isEmpty) {
          println("goint to match with p: "+properties)
          'Match
        }
        else if(!Drawing.selection.isEmpty) {
            //properties = Some(selection.shapes.head._2.attributes)
          println("settingh properties by seleciton: "+properties)
        }
        else {
          Siigna display("click shape to sample from")
          'Start
        }
      }


    },
    'Match -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End
      //add the properties to the selected shape


      //TODO: use selection to allow box selecting multiple shapes
      case MouseUp(p, _, _) :: tail => {
        if(nearestShape.isDefined && lessThanSelDist && properties.isDefined) {
          Drawing.select(nearestShape.get._1)
          Drawing.selection.addAttributes(properties.get)
          Drawing.deselect()
        }
      }
      case e =>

    }
  )
  override def paint(g: Graphics, t: TransformationMatrix) {
    val m = mousePosition
    //draw a customised cursor showing the properties to transfer, if set. Show a pipette otherwise
    if(!properties.isEmpty) g draw PolylineShape(Rectangle2D((m-Vector2D(3,3)),(m+Vector2D(3,3)))).addAttributes(properties.get)
    else g draw PolylineShape(Rectangle2D((m-Vector2D(3,3)),(m+Vector2D(3,3))))
  }
}
