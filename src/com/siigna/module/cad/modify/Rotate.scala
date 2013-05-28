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
import com.siigna.module.cad.create.InputRequestNew

class Rotate extends Module {

  private var anglePoint : Option[Vector2D] = None
  private var centerPoint : Option[Vector2D] = None

  private var transformation : TransformationMatrix = TransformationMatrix()
  def transformSelection(t : TransformationMatrix) = Drawing.selection.transform(t).shapes.values

  val stateMap: StateMap = Map(

    'Start -> {
      //exit mechanisms
      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End
      case End(KeyDown(Key.escape,modifier)) :: tail => End
      case MouseDown(p,MouseButtonRight,modifier) :: tail => End
      case KeyDown(Key.escape,modifier) :: tail => End

      // Receive a starting point from Input
      case End(p : Vector2D) :: tail => centerPoint = Some(p)
      case _ if centerPoint.isDefined => 'StartPoint

      // Quit if we get anything else from the input module
      case End(_) :: tail => End

      // If we are starting, forward to Input
      case Start(_, _) :: tail => {
        //Should be done differently, but this is how I can reach this (usableSelectionExists) function just quickly...
        if (Drawing.selection.isDefined) {
          Siigna display "set center point for rotation"
          Start('cad, "create.InputNew", InputRequestNew(6,None))
        } else {
          Siigna display "nothing selected"
          End
        }
      }
    },

    'StartPoint -> {
      case KeyDown(Key.Esc, _) :: tail => Deselect(); End

      // If the user drags the mouse we are searching for an angle
      case MouseMove(p1, _, _) :: MouseDown(p2, _, _) :: tail if (p1.distanceTo(p2) > 0.5) => {
        anglePoint = Some(p1)
        'DragAngle
      }
      // If the user clicks a single point, he defined the start of the rotation
      case MouseUp(p, _, _) :: _ :: MouseDown(_, _, _) :: tail => {
        anglePoint = Some(p)
        'AnglePoint
      }

      // If the user clicks a single point, he defined the start of the rotation
      case MouseUp(p, _, _) :: MouseDown(_, _, _) :: tail => {
        anglePoint = Some(p)
        'AnglePoint
      }

      // Match for input
      case KeyDown(c, _) :: tail if (c.toChar.isDigit) => {
        Start('cad, "create.InputOneValueByKey")
      }

      // Match for returning double
      case End(angle : Double) :: tail => {
        transformSelection(centerPoint.get, angle)
        Deselect()
        End
      }

      // If we get anything else we quit
      case End(_) :: tail => End
    },

    'AnglePoint -> {
      case MouseUp(p, _, _) :: tail => {
        transformSelection(centerPoint.get, getAngle(p) - getAngle(anglePoint.get))
        Drawing.deselect()
        End
      }
      case MouseMove(p, _, _) :: tail => {
        transformSelection(centerPoint.get, getAngle(p) - getAngle(anglePoint.get))
      }
      case KeyDown(Key.Esc, _) :: tail => Deselect(); End
    },

    'DragAngle -> {
      case MouseMove(p, _, _) :: tail => {
        transformSelection(centerPoint.get, getAngle(p) - getAngle(anglePoint.get))
      }
      case MouseUp(p, _, _) :: tail => {
        transformSelection(centerPoint.get, getAngle(p) - getAngle(anglePoint.get))
        Drawing.deselect()
        End
      }
      case KeyDown(Key.Esc,_) :: tail => Deselect(); End
    }
  )

  private def getAngle(p : Vector2D) = (p.transform(View.deviceTransformation) - centerPoint.get).angle

  private def transformSelection(center : Vector2D, angle : Double) = {
    Drawing.selection.transform(TransformationMatrix().rotate(angle, center))
  }

}