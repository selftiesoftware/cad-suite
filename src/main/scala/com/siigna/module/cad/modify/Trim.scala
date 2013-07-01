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
import util.geom.Geometry2D
import java.awt.Color

class Trim extends Module {

  /*
   * a function to trim a polylineShape
   * s1 = the trimGuideShape
   * s2 = the shape to be trimmed
   */

  def trimTwoShapes(s1 : Shape, s2 : Shape) = {
    if (s1.geometry.intersects(s2.geometry)) {
      val ints = s1.geometry.intersections(s2.geometry)
      println("s1 geom: "+s1.geometry)
      println("s2 geom: "+s2.geometry)
      println("ints: "+ints)
      ints

    } else println("NO INTERSECTION")
  }

  val trimGuideShapes : List[Shape] = List()

  val stateMap: StateMap = Map(
    //check if shapes are selected. If not, allow the user to do so.
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      //create testshapes
      case KeyDown(Key.ArrowDown, _) :: tail => {
        val lineVert = List(Vector2D(-100,0),Vector2D(10,10), Vector2D(100,0))
        val lineHoriz1 = List(Vector2D(-10,20),Vector2D(-15,-30))
        val lineHoriz2 = List(Vector2D(10,20),Vector2D(10,-30))

        Create(PolylineShape(lineVert))
        Create(PolylineShape(lineHoriz1))
        Create(PolylineShape(lineHoriz2))
      }
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      //save the selection if returned from the select module
      case End(MouseDown(p, MouseButtonLeft, _)) :: tail => {
        println("got selection")
      }

      case _ => {
        //goto trim if there is a selection only)
      }
    },

    //when shapes are selected, check for mouse clicks (later: also selection boxes(dragged)) to trim shapes.
    'Trim -> {
      case MouseDown(p, MouseButtonLeft, _) :: tail => {
        println("trim here!")
      }

      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End


      case _ => //stay in this state and check for new events
    }
  )
}