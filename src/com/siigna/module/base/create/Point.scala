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
  private var guideType : Option[Int] = None
  var pointPointGuide : Option[Vector2D => Traversable[Shape]] = None
  var pointDoubleGuide : Option[Double => Traversable[Shape]] = None
  var pointPointDoubleGuide : Option[Double => Traversable[Shape]] = None
  var pointPointPointGuide : Option[Vector2D => Traversable[Shape]] = None
  var sendPointPointGuide : Option[PointPointGuide] = None
  var sendPointDoubleGuide : Option[PointDoubleGuide] = None
  var sendPointPointDoubleGuide : Option[PointPointDoubleGuide] = None
  var sendPointPointPointGuide : Option[PointPointPointGuide] = None
  var snapAngle : Option[Double] = None
  val stateMap: StateMap = Map(

    'Start -> {
      //if InputTwoValue returns a vector, return it to the calling module:
      case End(p : Vector2D) :: tail => {
        End(p)
      }
      //if a single value is returned from InputOneValue of InputAngle, eturn it to the calling module:
      case End(s : Double) :: tail => {
        End(s)
      }

      //If an end command is recieved without input (from an input module):
      case End("no point returned") :: tail => {
        End("no point returned")
      }

      //If left mouse button is clicked: End and return mouse-position-point.
      case MouseDown(p,button,modifier)::tail => {
        if (button==MouseButtonLeft) {
          if (!sendPointDoubleGuide.isEmpty)  {
            val startPointX =sendPointDoubleGuide.get.point.x
            val startPointY =sendPointDoubleGuide.get.point.y
            val distanceFromStartToMouse: Double = math.sqrt(( (startPointX-mousePosition.transform(View.deviceTransformation).x) * (startPointX-mousePosition.transform(View.deviceTransformation).x)) + ( (startPointY-mousePosition.transform(View.deviceTransformation).y) * (startPointY-mousePosition.transform(View.deviceTransformation).y)) )
            if (distanceFromStartToMouse != 0) {
              End(distanceFromStartToMouse)
            }
          } else {
          End(p.transform(View.deviceTransformation))
        }} else {
          // In all other cases, the mouseDown is returned
          End(MouseDown(p,button,modifier))
        }
      }

      //If mouse up is recieved, and input-type is three:
      // THe mouse-up is returned.
      case MouseUp(p,button,modifier)::tail => {
        if (inputType.get == 3) {
          End(MouseUp(p,button,modifier))
        }
      }

      // Check for PointGuide - retrieve both the guide and its reference point, if it is defined.
      case Start(_ ,g : PointPointGuide) :: tail => {
        pointPointGuide = Some(g.pointGuide)
        inputType = Some(g.inputType)
        sendPointPointGuide = Some(g)
        guideType = Some(1)
      }

      // Check for PointDoubleGuide - retrieve both the guide and its reference point, if it is defined.
      case Start(_ ,g : PointDoubleGuide) :: tail => {
        pointDoubleGuide = Some(g.doubleGuide)
        inputType = Some(g.inputType)
        sendPointDoubleGuide = Some(g)
        guideType = Some(2)
      }

      // Check for PointPointDoubleGuide - retrieve both the guide and its reference point, if it is defined.
      case Start(_ ,g : PointPointDoubleGuide) :: tail => {
        pointPointDoubleGuide = Some(g.doubleGuide)
        inputType = Some(g.inputType)
        sendPointPointDoubleGuide = Some(g)
        guideType = Some(3)
      }

      // Check for PointPointPointGuide - retrieve both the guide and its reference point, if it is defined.
      case Start(_ ,g : PointPointPointGuide) :: tail => {
        pointPointPointGuide = Some(g.pointGuide)
        inputType = Some(g.inputType)
        sendPointPointPointGuide = Some(g)
        guideType = Some(3)
      }
      
      //If there is no guide, only the input type needs to be retrieved
      case Start(_,inp: Int) :: tail => {
        inputType = Some(inp)
      }
        
      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      //TODO: add if statement: if a track-guide is active, forward to a InputLength module instead...
      
      
      case KeyDown(key,modifier) :: tail => {

        //If the input is backspace with no modifiers, this key is returned to the asking module:
        if (key == Key.backspace && modifier == ModifierKeys(false,false,false)) {
          (End(KeyDown(key,modifier)))
        //If it is other keys, the input is interpreted by the input-modules.
        //Any existing guides are forwarded.
        } else if(inputType == Some(1)) {
            guide = false
            if (!sendPointPointGuide.isEmpty) Start('InputTwoValues,"com.siigna.module.base.create",sendPointPointGuide.get)
            else if (!sendPointDoubleGuide.isEmpty) Start('InputTwoValues,"com.siigna.module.base.create", sendPointDoubleGuide.get)
            else if (!sendPointPointDoubleGuide.isEmpty) Start('InputTwoValues,"com.siigna.module.base.create", sendPointPointDoubleGuide.get)
            else if (!sendPointPointPointGuide.isEmpty) Start('InputTwoValues,"com.siigna.module.base.create", sendPointPointPointGuide.get)
            else Start('InputTwoValues,"com.siigna.module.base.create")
        } else if(inputType == Some(2)) {
            guide = false
            if (!sendPointPointGuide.isEmpty) Start('InputOneValue,"com.siigna.module.base.create", sendPointPointGuide.get)
            else if (!sendPointDoubleGuide.isEmpty) Start('InputOneValue,"com.siigna.module.base.create", sendPointDoubleGuide.get)
            else Start('InputOneValue,"com.siigna.module.base.create")
        } else if(inputType == Some(3)) {
            guide = false
            if (!sendPointPointGuide.isEmpty) Start('InputAngle,"com.siigna.module.base.create", sendPointPointGuide.get)
            else if (!sendPointDoubleGuide.isEmpty) Start('InputAngle,"com.siigna.module.base.create", sendPointDoubleGuide.get)
            else Start('InputAngle,"com.siigna.module.base.create")
        } 
        
      }
      case _ => {
      }
    }
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    //draw the guide - but only if no points are being entered with keys, in which case the input modules are drawing.
    if ( guide == true) {
      //If a point is the desired return, x and y-coordinates are used in the guide
      if (!pointPointGuide.isEmpty) pointPointGuide.foreach(_(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t))))
      //If a double is the desired return, the distance from the starting point is used in the guide
      if (!pointDoubleGuide.isEmpty) {
        val startPointX =sendPointDoubleGuide.get.point.x
        val startPointY =sendPointDoubleGuide.get.point.y
        val distanceFromStartToMouse: Double = math.sqrt(( (startPointX-mousePosition.transform(View.deviceTransformation).x) * (startPointX-mousePosition.transform(View.deviceTransformation).x)) + ( (startPointY-mousePosition.transform(View.deviceTransformation).y) * (startPointY-mousePosition.transform(View.deviceTransformation).y)) )
        pointDoubleGuide.foreach(_(distanceFromStartToMouse).foreach(s => g.draw(s.transform(t))))
      }
      if (!pointPointPointGuide.isEmpty) {
        pointPointPointGuide.foreach(_(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t))))
      }
    }
  }
}

/**
 * inputType (Int) is passed on to text input modules if values are typed.
 * possible values:
 * 1 = x and y coordinates  (handled by the InputTwoValues module) - any input
 * 2 = one length value     (handled by the InputOneValue module) -any input
 * 3 = x and y coordinates  (handled by the InputTwoValues module) - key-input, or on MouseUp
 * 
 * Guide types depend on what the start and end types are. They are:
 * 1 =  PointGuide:      Start:        Vector2D    End:     Vector2D
 * 2 =  PointDoubleGuide:     Start:        Vector2D    End:     Double
 * 3 =  PointPointDouble:     Point1 and 2: Vector2d    End: Double
 */

//The basic point guide - a vector2D is the base for the shapes
case class PointGuide(pointGuide : Vector2D => Traversable[Shape] , inputType : Int)

//The basic double guide - a double is the base for the shapes
case class DoubleGuide(pointGuide : Double => Traversable[Shape] , inputType : Int)

//A point and a point guide - a vector2D delivered along a basic point guide,
// for use when the guide needs to relate to a fixed point
case class PointPointGuide(point : Vector2D , pointGuide : Vector2D => Traversable[Shape] , inputType : Int)

//A point and a double guide - a vector2D delivered along a basic double guide,
// for use when the guide needs to relate to a fixed point
case class PointDoubleGuide(point : Vector2D , doubleGuide : Double => Traversable[Shape] , inputType : Int)

//two points and a point guide - two vector2Ds delivered along a basic point guide,
// for use when the guide needs to relate to two fixed points
case class PointPointDoubleGuide(point1 : Vector2D, point2 : Vector2D, doubleGuide : Double => Traversable[Shape] , inputType : Int)

//two points and a double guide - two vector2Ds delivered along a basic double guide,
// for use when the guide needs to relate to two fixed points
case class PointPointPointGuide(point1 : Vector2D, point2 : Vector2D, pointGuide : Vector2D => Traversable[Shape] , inputType : Int)


