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

  //STRATEGY:
  //1) if sel.size == 2
  //2) get endPoints
  //3) if coinsiding ends: Create(newS = s1+s2)
  //4)del s1 and s2
  //5) select newS
  //6) Start(Select)

  //a function to merge two shapes if an end coinside.
  //returns None if this is not the case
  def endsCheck(s1 : Shape, s2 : Shape) : Boolean = {
    val start1 = s1.geometry.vertices.head
    val end1 = s1.geometry.vertices.last
    val start2 = s1.geometry.vertices.head
    val end2 = s1.geometry.vertices.last

    //check for coinsiding ends and join shapes if such are found
    if(start1 == start2 || end1 == start1 || end1 == end2 || end1 == start2) true else false
  }

  def addTwoVerticeLists(l1 : List[Vector2D], l2 : List[Vector2D]) : List[Vector2D] = {
    //println("l1: "+l1)
    //println("l2: "+l2)

    //align list1
    val newL1 = if(l1.head == l2.head) {
      l1.reverse
    } else l1

    //align list2
    val newL2 = if(l1.last == l2.last) {
      l2.reverse
    } else l2

    newL1 ++ newL2
    if(l1.head == l2.last) newL1.reverse ++ newL2.reverse else newL1 ++ newL2 //prevent false construction of closed PLs
  }

  var selection = Selection()

  var s1vertices : List[Vector2D] = List()
  var s2vertices : List[Vector2D] = List()

  val stateMap: StateMap = Map(

    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case _ => {

        val selectionShapes = Drawing.selection.shapes.map(s => s._2)
        //1) if sel.size == 2
        if (selectionShapes.size != 2) {
          Siigna display "Select two objects to join"
          Start('cad, "Selection")
        } else {
          selection = Drawing.selection //save the selection so that the original shapes can be deleted

          Siigna display "joining shapes"

          //2) get endPoints
          val s1 = selectionShapes.head
          val s2 = selectionShapes.last

          //check that the shapes are line or polyline shapes
          if(s1.isInstanceOf[LineShape] || s1.isInstanceOf[PolylineShapeOpen]) {
            if(s2.isInstanceOf[LineShape] || s2.isInstanceOf[PolylineShapeOpen]) {

              //if ends coinside, join the shapes
              if(endsCheck(s1,s2)) {
                //get the vertices of line one
                s1 match {
                  case l : LineShape => s1vertices = List(l.p1,l.p2)
                  case p : PolylineShapeOpen => s1vertices = p.geometry.vertices.toList
                  case _ => {
                    Siigna display "Oooops, something wrong in join with shape 1"
                    End
                  }
                }

                //get the vertices of line two
                s2 match {
                  case l : LineShape => s2vertices = List(l.p1,l.p2)
                  case p : PolylineShapeOpen => s2vertices = p.geometry.vertices.toList
                  case _ => {
                    Siigna display "Oooops, something wrong in join with shape 2"
                    End
                  }
                }
                //join (poly)lines
                val s = addTwoVerticeLists(s1vertices,s2vertices)
                Create(PolylineShape(s))
                //5)del s1 and s2
                if(!selection.isEmpty) Delete(selection)
                //6) select newS
                val id = if(Siigna.latestID.isDefined) Siigna.latestID else None
                if(id.isDefined) Select(id)

                //7) Start(Select) to look for new shapes to join
                Start('cad, "Selection")

              }
            } else Siigna display "joining works for lines only"
          }
        }
        //End the module
        End
      }
    }
  )
}
