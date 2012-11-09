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

/**
 * The point module answers to requests from many other modules who require a number or one or more points to function.
 *
 */

class Point extends Module {

  //VARS declaration:
  private var decimalValue : Boolean = false
  private var point : Option[Vector2D] = None
  var pointGuide : Option[Vector2D => Traversable[Shape]] = None
  var sendGuide : Option[PointGuide] = None
  var snapAngle : Option[Double] = None
  val stateMap: StateMap = Map(

    'Start -> {
      case End(p : Vector2D) :: tail => {
        End(p)
      }

      case MouseDown(p,button,modifier)::tail => {
        // End and return mouse-position-point, if left mouse button is clicked
        if (button==MouseButtonLeft) {
          End(p.transform(View.deviceTransformation))
        } else {
          // In all other cases, the mouseDown is returned
          End(MouseDown(p,button,modifier))
        }
      }
      // Check for PointGuide
      case Start(_ ,g : PointGuide) :: tail => {
        pointGuide = Some(g.guide)
        sendGuide = Some(g)
      }
      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      // End and return backspace, if backspace is pressed
      //case KeyDown(Key.Backspace, _) :: tail => {
      //  println("Backspace pressed")
      //  End(KeyDown(Key.Backspace,ModifierKeys(false,false,false)))
      //}
      // All other keyDown events are forwarded:

      //TODO: the first key-input is lost... it should be used in the coords module.
      case KeyDown(key,modifier) :: tail => {
        //TODO: add if statement: if a track-guide is active, forward to a InputLength module instead...

        if(pointGuide.isDefined) {
          Start('InputTwoValues,"com.siigna.module.base.create", sendGuide.get)
        }
        else Start('InputTwoValues,"com.siigna.module.base.create")

        //End(KeyDown(key,modifier))
      }
      case _ => {
      }
    }
  )
  //TODO: disable this pointGuide if the InputTwoValues module is used
  override def paint(g : Graphics, t : TransformationMatrix) {
    pointGuide.foreach(_(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t))))
  }
}

/**
 * A class used to draw guides in the point module.
 * coordType (Int) is passed on to text input modules if values are typed.
 * possible values:
 * 1 = x and y coordinates  (handled by the InputTwoValues module)
 * 2 = one length value     (handled by the InputLength module)
 * 3 = one angle value      (handled by the InputAngle module)
 */
case class Guide(guide : Vector2D => Traversable[Shape])

//a case class where the point can be extracted.
case class PointGuide(point : Vector2D , guide : Vector2D => Traversable[Shape])