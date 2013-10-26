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

  def joinLines(s : Map[Int, Shape]) : Map[Int, Shape] = {
    //an optimised list of ids and shape start,end to evaluate for coinsiding points.
    var idsAndEndPoints : Map[Int, List[Vector2D]] = Map()

    //TODO: a recursive function to join all (poly)lines with coinsiding endpoints.


    //shorten long polylines so that the join evaluation uses the first and last point only.
    def shorten(i : Int, s : PolylineShapeOpen) : (Int,List[Vector2D]) = (i,List(s.startPoint,s.last.geometry.vertices.last.vertices.last))

    s.foreach(s => s._2 match {
        //filter out irrelevant shapes
        case p : PolylineShapeOpen => idsAndEndPoints = idsAndEndPoints + shorten(s._1,p)
        println("ids and endpoints: "+idsAndEndPoints)



        //TODO: can not add these (i,List(a,b)) to the Map??!?
        //case l : LineShape => idsAndEndPoints = idsAndEndPoints + (s._1,List(l.start,l.end))
        case _ => println("dismissing shape; "+s)
      }
    )

    //evaluate the start and end points of the shapes.
    println("optimised list to evaluate; "+idsAndEndPoints)

    //TODO: write a join algorithm which joins the shapes with coinsiding endpoints...


    //return
    s
  }

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

          val shapes = Drawing.selection.shapes
          //run the joinLines function
          joinLines(shapes)
        }
        //End the module
        End
      }
    }
  )
}
