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

import com.siigna.module.{ModuleInit, Module}
import com.siigna._
import com.siigna.util.event.{End, MouseDown, KeyDown}
import com.siigna.module.cad.create.{DoubleGuide, InputRequest, Vector2DGuide}

class Mirror extends Module {

  var endPoint : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None
  var transformation : TransformationMatrix = TransformationMatrix()
  def transformSelection(t : TransformationMatrix) = Drawing.selection.transform(t).shapes.values
  val origin = Drawing.selection.transformation

  val stateMap: StateMap = Map(

    //find the first mirror point
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case End(p : Vector2D) :: tail => {
        println("got startPoint: "+p)

      }
      //look for the first point on the mirror line
      case _ => Start('cad, "create.Input", InputRequest(6,None))


    },
     //find the second mirror point
    'Mirror -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case MouseMove(p, _, _) :: tail => {
      println("in mirror state, draw dynamically")
      }
      case End(p : Vector2D) :: tail => {
        println("got endPoint - do mirror operation")

      }
      case _ => //wait for input
    },
    'End -> {
      case _ => End
    }
  )
}
