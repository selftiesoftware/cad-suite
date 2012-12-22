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
 */

class Input extends Module {

  //VARS declaration:
  var inputRequest: Option[InputRequest] = None
  var vector2DGuide: Option[Vector2DGuide] = None
  var doubleGuide: Option[DoubleGuide] = None
  var textGuide: Option[TextGuide] = None
  var vector2DMessageGuide: Option[Vector2DMessageGuide] = None
  var doubleMessageGuide: Option[DoubleMessageGuide] = None
  var textMessageGuide: Option[TextMessageGuide] = None
  var referencePoint1: Option[Vector2D] = None
  var referencePoint2: Option[Vector2D] = None
  var referenceDouble: Option[Double] = None
  var inputType: Option[Int] = None

  private var guide : Boolean = true
  private var guideKeepGuideOff: Boolean = true

  var snapAngle : Option[Double] = None

  val stateMap: StateMap = Map(

    'Start -> {

      //Check for input request:
      case Start(_ , i: InputRequest) :: tail => {
        inputRequest = Some(i)
        if (!i.vector2DGuide.isEmpty) vector2DGuide = i.vector2DGuide
        if (!i.doubleGuide.isEmpty) doubleGuide = i.doubleGuide
        if (!i.textGuide.isEmpty) textGuide = i.textGuide
        if (!i.vector2DMessageGuide.isEmpty) vector2DMessageGuide = i.vector2DMessageGuide
        if (!i.doubleMessageGuide.isEmpty) doubleMessageGuide = i.doubleMessageGuide
        if (!i.textMessageGuide.isEmpty) textMessageGuide = i.textMessageGuide
        if (!i.referencePoint1.isEmpty) referencePoint1 = i.referencePoint1
        if (!i.referencePoint2.isEmpty) referencePoint2 = i.referencePoint2
        if (!i.referenceDouble.isEmpty) referenceDouble = i.referenceDouble
        if (!i.inputType.isEmpty) inputType = i.inputType
      }

      //If there is no guide, only the input type needs to be retrieved
      case Start(_,inp: Int) :: tail => {
        inputType = Some(inp)
      }


      //if InputTwoValue returns a vector, return it to the calling module:
      case End(p : Vector2D) :: tail => {
        if (inputType == Some(1) || inputType == Some(2) || inputType == Some(111) ) {
          End(p)
        } else if (inputType == Some(102) || inputType == Some(1020)) {
          End(MouseUp(p,MouseButtonLeft,ModifierKeys(false,false,false)))
        } else if (inputType == Some(1021)) {
          End(MouseDown(p,MouseButtonLeft,ModifierKeys(false,false,false)))
        } else if (inputType == Some(112) && !referencePoint1.isEmpty) {
          End(referencePoint1.get + p)
        } else if (inputType == Some(17)) {
          End
        }
      }
      //if a single value is returned from InputOneValue or InputAngle, return it to the calling module:
      case End(s : Double) :: tail => {
        if (inputType == Some(3) || inputType == Some(4) || inputType == Some(5) || inputType == Some(6)
          || inputType == Some(7) || inputType == Some(8) || inputType == Some(9) || inputType == Some(10)
          || inputType == Some(12) || inputType == Some(13) || inputType == Some(131)
          || inputType == Some(17) || inputType == Some(103) || inputType == Some(1031)) {
          End(s)
        } else if ((inputType == Some(16) || inputType == Some(111)|| inputType == Some(112)) && Track.isTracking == true) {
          End(Track.getPointFromDistance(s).get)
        }
      }

      //if a string is returned, return it to the calling module:
      case End(s : String) :: tail => {
        End(s)
      }

      //If a mouseDown is returned (angle gizmo does that)  
      case End(MouseDown(p,button,modifier)) :: tail => {
        End(p)
      }
        
      //If left mouse button is clicked: End and return mouse-position-point.
      case MouseDown(p,button,modifier)::tail => {
        if (button==MouseButtonLeft) {
          if (inputType == Some(1) || inputType == Some (11) || inputType == Some (12) || inputType == Some (13)
            || inputType == Some(131) || inputType == Some (111) || inputType == Some (112) || inputType == Some(1031)) {
            End(p.transform(View.deviceTransformation))
          } else if (inputType == Some(2) || inputType == Some(4) || inputType == Some(6) | inputType == Some(8))  {
            referencePoint1 = Some(p)
            //Start painting, if it has been turned off
            if (guideKeepGuideOff == true) guideKeepGuideOff = false
          } else if (inputType == Some(3))  {
            //Type is distance from start point, returned as double
            val startPointX = referencePoint1.get.x
            val startPointY = referencePoint1.get.y
            val distanceFromStartToMouse: Double = math.sqrt(( (startPointX-mousePosition.transform(View.deviceTransformation).x) * (startPointX-mousePosition.transform(View.deviceTransformation).x)) + ( (startPointY-mousePosition.transform(View.deviceTransformation).y) * (startPointY-mousePosition.transform(View.deviceTransformation).y)) )
            if (distanceFromStartToMouse != 0) {
              End(distanceFromStartToMouse)
            }
          } else if (inputType == Some(5))  {
            End(p.transform(View.deviceTransformation).x)
          } else if (inputType == Some(7)) {
            End(p.transform(View.deviceTransformation).y)
          } else if (inputType == Some(102) || inputType == Some(1020) || inputType == Some(1021) 
            || inputType == Some(103))  {
            //The mouseDown is saved as point1, if it does not already exist
            //If there is a mouseUp later, on the same point, the point is returned as a mouseDown (happens in mouseUp-part)
            if (referencePoint1.isEmpty) referencePoint1 = Some(p)
            //Start drawing the guide - necessary if it will be dragged.
            if (guideKeepGuideOff == true) guideKeepGuideOff = false
          } else if (inputType == Some(17)) {
            End
          }
        //Right mouse button:
        } else {
          if (inputType == Some(17)) {
            End
          } else { 
            // In all other cases, where it is not left mouse button, the mouseDown is returned
          End(MouseDown(p.transform(View.deviceTransformation),button,modifier))
          }
        }
      }

      //If mouse up is received,
      case MouseUp(p,button,modifier)::tail => {
        if (inputType.get == 2) {
          End(Vector2D((p - referencePoint1.get).x,-(p - referencePoint1.get).y))
        } else if (inputType.get == 4) {
          //Type is distance from start point, returned as double
          val startPointX = referencePoint1.get.x
          val startPointY = referencePoint1.get.y
          val distanceFromStart: Double = math.sqrt(( (startPointX-p.transform(View.deviceTransformation).x) * (startPointX-p.transform(View.deviceTransformation).x)) + ( (startPointY-p.transform(View.deviceTransformation).y) * (startPointY-p.transform(View.deviceTransformation).y)) )
          if (distanceFromStart != 0) {
            End(distanceFromStart)
          }
        } else if (inputType.get == 6) {
          End(p.x - referencePoint1.get.x)
        } else if (inputType.get == 8) {
          End(p.y - referencePoint1.get.y)
        } else if (inputType.get == 8) {
          End(MouseUp(Vector2D((p - referencePoint1.get).x,-(p - referencePoint1.get).y),MouseButtonLeft,ModifierKeys(false,false,false)))
        } else if (inputType.get == 9) {
          End(p.transform(View.deviceTransformation))
        } else if (inputType.get == 102 || inputType == Some(1020) || inputType == Some(1021)) {
          //If mouseUp occurs on the same point as mouseDown, the point is returned as a mouseDown event.
          //If mouseUp occurs on a different point, coordinates from mouseDown to up is returned as a mouseUp event.
          if (!referencePoint1.isEmpty) {
            if (p == referencePoint1.get)
              End(MouseDown(p.transform(View.deviceTransformation),button,modifier))
            else  End(MouseUp(Vector2D((p - referencePoint1.get).x,-(p - referencePoint1.get).y),button,modifier))
          }
        } else if (inputType.get == 103) {
          if (!referencePoint1.isEmpty) {
            if (p == referencePoint1.get)
              End(MouseDown(p.transform(View.deviceTransformation),button,modifier))
            //If it is a new point, a drag has occurred, and the length of that drag is returned
            else  End(Vector2D((p - referencePoint1.get).x,-(p - referencePoint1.get).y).length)
          }
        }
      }

      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      //TODO: add if statement: if a track-guide is active, forward to a InputLength module instead...

      case KeyDown(key,modifier) :: tail => {
        //If the input is backspace with no modifiers, this key is returned to the asking module:
        if (key == Key.backspace && modifier == ModifierKeys(false,false,false)) {
          (End(KeyDown(key,modifier)))

          //if SHIFT is pressed, forward to the Angle Gizmo -
          //but only if there is a reference point: Either point1, or a tracked point:
        } else if(key == Key.shift && (inputType == Some(1) || inputType == Some(111) || inputType == Some(112))
                          && (!referencePoint1.isEmpty || (Track.isTracking == true && Track.pointOne.get.distanceTo(mousePosition.transform(View.deviceTransformation)) < Siigna.selectionDistance))) {
          //Start angle gizmo, and send the the active guide.
          //The gizmo draws guide, so input should not.
          if (guide == true) guide = false
          if (!vector2DGuide.isEmpty) Start('AngleGizmo,"com.siigna.module.base.create",inputRequest.get)
          else Start('AngleGizmo,"com.siigna.module.base.create")

          //If it is other keys, the input is interpreted by the input-modules.
          //Any existing guides are forwarded.
        } else if (key == Key.shift) { //Do nothing if shift is pressed, but there is no point to start the angleGizmo from
        } else if(inputType == Some(1) || inputType == Some(2) || inputType == Some(102) || inputType == Some(1020)
                  || inputType == Some(1021)
                  || ((inputType == Some(16) || inputType == Some(111) || inputType == Some(112)) && Track.isTracking == false)) {
            if (guide == true) guide = false
            if (!inputRequest.isEmpty) Start('InputTwoValues,"com.siigna.module.base.create",inputRequest.get)
            else Start('InputTwoValues,"com.siigna.module.base.create")
        } else if(inputType == Some(3) || inputType == Some(4) || inputType == Some(5) || inputType == Some(6) 
                  || inputType == Some(7) || inputType == Some(8) || inputType == Some(10) || inputType == Some(12) 
                  || inputType == Some(13) || inputType == Some(131) || inputType == Some(16) || inputType == Some(17)
                  || inputType == Some(103)    || inputType == Some(111) || inputType == Some(112) || inputType == Some(1031)) {
            if (guide == true) guide = false
            if (!inputRequest.isEmpty) Start('InputOneValue,"com.siigna.module.base.create",inputRequest.get)
            else Start('InputOneValue,"com.siigna.module.base.create")
        } else if(inputType == Some(14) ) {
            if (guide == true) guide = false
            if (!inputRequest.isEmpty) Start('InputText,"com.siigna.module.base.create",inputRequest.get)
            else Start('InputText,"com.siigna.module.base.create")
        }
      }
      case _ => {
      }
    }
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    if (inputType == Some(12)) guide = false
    if (inputType == Some(1020)) {
      if (guideKeepGuideOff == true) guide = false
      else guide = true
    }
    //draw the guide - but only if no points are being entered with keys, in which case the input modules are drawing.
    if ( guide == true) {
      if (!vector2DGuide.isEmpty) vector2DGuide.get.vector2DGuide(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t)))
    }
  }
}

/**
 * inputType (Int) lets the modules tell, what return they accept:
 * Returned            AngleGizmo     Input method:
 * variable type      (x: Activated)  How the returned variable is produced:
 * 1 = Vector2D                    x  MouseDown, Key (absolute - handled by the InputTwoValues module)
 * 2 = Vector2D                       Coordinates from mouseDown to mouseUp, or Key (absolute - handled by the InputTwoValues module)
 * 3 = Double                         Distance from given start point to point given by mouseDown, or Key (handled by the InputOneValues module)
 * 4 = Double                         Distance from mouse down to mouse up, or Key (handled by the InputOneValues module)
 * 5 = Double                         x-coordinate from mouseDown, or Key
 * 6 = Double                         x-coordinate difference from mouse Down to mouseUp, or key
 * 7 = Double                         y-coordinate from mouseDown, or Key
 * 8 = Double                         y-coordinate difference from mouse Down to mouseUp, or key
 * 9 = Vector2D                       Coordinates at mouseUp
 * 10 = Double                        Key input only
 * 11 = Vector2D                      Left mouse click only
 * 12 = Double                        Key (one value)
 *      Vector2D                      MouseDown
 *      Special guide:                Do not draw guide
 * 13 = Double                        Key (one value)
 *      Vector2D                      MouseDown. Guide is drawn.
 * 131 =Double                        Key (one value)
 *      Vector2D                      MouseDown. Guide is drawn.
 *      Special guide:                In InputOneValue - for dynamically drawing offset of shapes.
 * 14 = String                        Key input, text
 * 15 = Nothing                       Returns nothing from Input module. Can for example be used when calling inputOne or two value modules from other modules than input.
 * 16 = Vector2D                      Key input, one-coordinate, offset from existing point when on a track guide
 * 17 = Double                        Key - InputOneValue
 *      End                           All other inputs sends End
 *
 * 111 = Vector2D                  x  Point at mouseDown, or point by key(absolute - twoValues) or point guided by trackguide (input One value) if a track guide is active.
 * 112 = Vector2D                  x  Point at MouseDown, or (point by key added to referencePoint1) or point guided by trackguide (input One value) if a track guide is active.
 *
 * 102 = mouseDown, with Vector2D     MouseDown (sent after mouseUp received)
 *       mouseUp, with Vector2D       coordinates from mouseDown to mouseUp, Key (absolute - handled by the InputTwoValues module)
 * 1020 = mouseDown, with Vector2D    MouseDown (sent after mouseUp received)
 *        mouseUp, with Vector2D      coordinates from mouseDown to mouseUp, Key (absolute - handled by the InputTwoValues module)
 *        Special guide:              Do not draw guide in input until left mouse button is clicked.
 * 1021 = mouseDown, with Vector2D    MouseDown (sent after mouseUp received), Key (absolute - handled by the InputTwoValues module)
 *       mouseUp, with Vector2D       coordinates from mouseDown to mouseUp
 * 103 = Double                       Length of vector from mouseDown to mouseUp, or key-input
 *       mouseDown, with Vector2D     mouseDown (sent after mouseUp received)
 * 1031 = Double                      key-input
 *        Vector2D                    Point at mouseDown

 * 
 * 
 */

//The guides:
case class DoubleGuide(doubleGuide : Double => Traversable[Shape])
case class Vector2DGuide(vector2DGuide : Vector2D => Traversable[Shape])
case class TextGuide(textGuide : String => Traversable[Shape])

case class DoubleMessageGuide(doubleMessageGuide : Double => Unit)
case class Vector2DMessageGuide(vector2DMessageGuide : Vector2D => Unit)
case class TextMessageGuide(textMessageGuide : String => Unit)

//The input request:
case class InputRequest(vector2DGuide: Option[Vector2DGuide] = None,                   //1
                        doubleGuide: Option[DoubleGuide] = None,                       //2
                        textGuide: Option[TextGuide] = None,                           //3
                        vector2DMessageGuide: Option[Vector2DMessageGuide] = None,     //4
                        doubleMessageGuide: Option[DoubleMessageGuide] = None,         //5
                        textMessageGuide: Option[TextMessageGuide] = None,             //6
                        referencePoint1: Option[Vector2D] = None,                      //7
                        referencePoint2: Option[Vector2D] = None,                      //8
                        referenceDouble: Option[Double] = None,                        //9
                        inputType: Option[Int] = None)                                 //10
