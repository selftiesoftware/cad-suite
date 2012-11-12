/*
* Copyright (c) 2012. Siigna is released under the creative common license by-nc-sa. You are free
* to Share — to copy, distribute and transmit the work,
* to Remix — to adapt the work
*
* Under the following conditions:
* Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
* Noncommercial — You may not use this work for commercial purposes.
* Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
*/

package com.siigna.module.base.create

import com.siigna._
import java.nio.file.OpenOption

/**
 * The point module answers to requests from many other modules who require a number or one or more points to function.
 *
 */

class Point extends Module {

  //VARS declaration:
  private var decimalValue : Boolean = false
  private var point : Option[Vector2D] = None
  private var guide : Boolean = true
  private var inputType : Option[Int] = None
  var pointGuide : Option[Vector2D => Traversable[Shape]] = None
  var sendGuide : Option[PointGuide] = None
  var snapAngle : Option[Double] = None
  val stateMap: StateMap = Map(

    'Start -> {
      //if InputTwoValue returns a vector, return it to the calling module:
      case End(p : Vector2D) :: tail => {
        End(p)
      }
      //if a single value is returned from InputOneValue of InputAngle, eturn it to the calling module:
      case End(l : Double) :: tail => {
        End(l)
      }

      //If left mouse button is clicked: End and return mouse-position-point.
      case MouseDown(p,button,modifier)::tail => {
        if (button==MouseButtonLeft) {
          End(p.transform(View.deviceTransformation))
        } else {
          // In all other cases, the mouseDown is returned
          End(MouseDown(p,button,modifier))
        }
      }
      // Check for PointGuide - retrieve both the guide and its reference point, if it is defined.
      case Start(_ ,g : PointGuide) :: tail => {
        pointGuide = Some(g.guide)
        inputType = Some(g.inputType)
        sendGuide = Some(g)
      }
      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      //TODO: add if statement: if a track-guide is active, forward to a InputLength module instead...
      case KeyDown(key,modifier) :: tail => {
        guide = false
        if(pointGuide.isDefined && inputType == Some(1)) {
          Start('InputTwoValues,"com.siigna.module.base.create", sendGuide.get)
        }
        else if(pointGuide.isDefined && inputType == Some(2)) {
          Start('InputOneValue,"com.siigna.module.base.create", sendGuide.get)
        }
        else if(pointGuide.isDefined && inputType == Some(3)) {
          Start('InputAngle,"com.siigna.module.base.create", sendGuide.get)
        }
        else None
      }
      case _ => {
      }
    }
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    //draw the guide - but only if no points are being entered with keys, in which case the input modules are drawing.
    if ( guide == true) {
      pointGuide.foreach(_(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t))))
    }
  }
}



case class Guide(guide : Vector2D => Traversable[Shape])

/**
 * a case class for sending a point guide from which the point can be extracted, and the input type is defined.
 * inputType (Int) is passed on to text input modules if values are typed.
 * possible values:
 * 1 = x and y coordinates  (handled by the InputTwoValues module)
 * 2 = one length value     (handled by the InputOneValue module)
 */

case class PointGuide(point : Vector2D , guide : Vector2D => Traversable[Shape] , inputType : Int)