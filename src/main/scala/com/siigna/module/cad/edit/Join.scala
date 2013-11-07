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
import app.model.shape.PolylineShape.PolylineShapeOpen
import app.Siigna

class Join extends Module{

  /**
   * a function to join the lines and polylines which can be joined within a list of shapes.
   * input: a list of shapes
   * return: the joined shapes
   * old shapes are deleted.
   */

  //strategy: evaluate the shapes two by two recursively.
  //If two shapes have coinsiding endpoints, join them into a polyline shape.
  //update the selection, adding the new shape and deleting the two old ones
  //run the evaluation again
  //end if no shapes have coinciding ends.

  //a function to merge two shapes if an end coinside.
  //returns None if this is not the case
  def endsCheck(s1 : Shape, s2 : Shape) = {
    val start1 = s1.geometry.vertices.head
    val end1 = s1.geometry.vertices.last
    val start2 = s1.geometry.vertices.head
    val end2 = s1.geometry.vertices.last

    //check for coinsiding ends and join shapes if such are found
    if(start1 == start2 || end1 == start1 || end1 == end2 || end1 == start2) joinedShapes = List(s1)
  }

  //run the endsCheck function
  def evaluateShapes(shapes : List[Shape]) = {
    //evaluate if the first shape in the list is connected to any of the other shapes
    if(shapes.length > 0) shapes.foreach(s => endsCheck(shapes.head,s))

    //remove the shape from the original list
    //evalShapes = shapes.tail
  }

  var evalShapes : List[Shape] = List()
  var joinedShapes : List[Shape] = List()

  val stateMap: StateMap = Map(

    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case _ => {
        if (Drawing.selection.isEmpty) {
          Siigna display "Select objects to join"
          Start('cad, "Selection")
        } else {
          Siigna display "joining shapes"

          //evalShapes = Drawing.selection.shapes.toList
          //loop until the original shapes list is empty
          //while(evalShapes.length > 0) evalShapes.map(e => e.evaluateShapes(evalShapes)

        }
        //End the module
        End
      }
    }
  )
}
