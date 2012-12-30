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
  private var drawGuide : Boolean = true
  private var turnGuideOn : Boolean = false
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
  var mouseDownPoint: Option[Vector2D] = None

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
      //If there is no input request, only the input type needs to be retrieved
      case Start(_,inp: Int) :: tail => {
        inputType = Some(inp)
      }

      //Input from mouse-actions:

      //Left mouse button down (Standard: the clicked point is returned, transformed to view):
      case MouseDown(p,MouseButtonLeft,modifier)::tail => {
        if (inputType == Some(2) || inputType == Some(4) || inputType == Some(6) | inputType == Some(8))  {
          referencePoint1 = Some(p)
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
        } else if (inputType == Some(102) || inputType == Some(1020) || inputType == Some(1021) || inputType == Some(120)
          || inputType == Some(103))  {
          //The mouseDown is saved as referencePoint1, if it does not already exist, for later processing - after mouse-up is recieved
          mouseDownPoint = Some(p)
          //If guide should be drawn after the first mouse-down:
          if (inputType == Some(1020) || inputType == Some(120)) turnGuideOn = true
        } else if (inputType == Some(17)) {
          End
        } else {
          //Standard: the clicked point is returned, transformed to view
          End(p.transform(View.deviceTransformation))
        }
      }
      //Right mouse button down (Standard: the mouseDown action is returned)
      case MouseDown(p,MouseButtonRight,modifier)::tail => {
        //Standard: the mouseDown action is returned
        End(MouseDown(p.transform(View.deviceTransformation),MouseButtonRight,modifier))
      }
      //Left mouse button up: (Standard: Nothing happens)
      case MouseUp(p,MouseButtonLeft,modifier)::tail => {
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
          if (!mouseDownPoint.isEmpty) {
            if (p == mouseDownPoint.get)
              End(MouseDown(p.transform(View.deviceTransformation),MouseButtonLeft,modifier))
            else  End(MouseUp(Vector2D((p - referencePoint1.get).x,-(p - referencePoint1.get).y),MouseButtonLeft,modifier))
          }
        } else if (inputType.get == 103) {
          if (!mouseDownPoint.isEmpty) {
            if (p == mouseDownPoint.get)
              End(MouseDown(p.transform(View.deviceTransformation),MouseButtonLeft,modifier))
            //If it is a new point, a drag has occurred, and the length of that drag is returned
            else  End(Vector2D((p - referencePoint1.get).x,-(p - referencePoint1.get).y).length)
          }
        } else if (inputType == Some(120)) {
          if (!mouseDownPoint.isEmpty) {
            if (p == mouseDownPoint.get)
              End(p.transform(View.deviceTransformation))
            else if (!referencePoint1.isEmpty)
              End(-((p.transform(View.deviceTransformation) - referencePoint1.get).angle - (mouseDownPoint.get.transform(View.deviceTransformation) - referencePoint1.get).angle))
            else println("ReferencePoint1 required")
        }}
      }

      //Input from keyboard:

      //Most key-inputs are not handled directly in Input, but sorted and forwarded to key-input modules.
      //Some are, however - eg. escape and backspace.
      case KeyDown(key,modifier) :: tail => {
        //ESCAPE: Is returned to the asking module as a key-down event:
        if (key == Key.escape) End(KeyDown(key,modifier))
        //BACKSPACE with no modifiers: Is returned to the asking module as a key-down event:
        else if (key == Key.backspace && modifier == ModifierKeys(false,false,false)) {
          (End(KeyDown(key,modifier)))
        //SHIFT: (Standard: Nothing happens)
        //If it is an input type with activated angleGizmo, forward to the Angle Gizmo -
        //but only if there is a reference point: Either point1, or a tracked point:
        } else if(key == Key.shift && (inputType == Some(1) || inputType == Some(111) || inputType == Some(112))
          && (!referencePoint1.isEmpty || (Track.isTracking == true && Track.pointOne.get.distanceTo(mousePosition.transform(View.deviceTransformation)) < Siigna.selectionDistance))) {
          //If it is an input type with activated angleGizmo: Start angleGizmo, and send the the input request.
          //The gizmo draws guide, so input should not.
          if (drawGuide == true) drawGuide = false
          if (!vector2DGuide.isEmpty) Start('AngleGizmo,"com.siigna.module.base.create",inputRequest.get)
          else Start('AngleGizmo,"com.siigna.module.base.create")
        //Do nothing if shift is pressed and the angleGizmo shouldn't start:
        } else if (key == Key.shift) {
        //OTHER KEYS: The inputRequest is forwarded to the input-modules for interpretation according to input-type:
        } else if(inputType == Some(1) || inputType == Some(2) || inputType == Some(102) || inputType == Some(1020)
          || inputType == Some(1021)
          || ((inputType == Some(16) || inputType == Some(111) || inputType == Some(112)) && Track.isTracking == false)) {
          if (drawGuide == true) drawGuide = false
          if (!inputRequest.isEmpty) Start('InputTwoValues,"com.siigna.module.base.create",inputRequest.get)
          else Start('InputTwoValues,"com.siigna.module.base.create")
        } else if(inputType == Some(3) || inputType == Some(4) || inputType == Some(5) || inputType == Some(6)
          || inputType == Some(7) || inputType == Some(8) || inputType == Some(10) || inputType == Some(12)
          || inputType == Some(13) || inputType == Some(16) || inputType == Some(17)
          || inputType == Some(103)    || inputType == Some(111) || inputType == Some(112) || inputType == Some(1031)
          || inputType == Some(120)) {
          if (drawGuide == true) drawGuide = false
          if (!inputRequest.isEmpty) Start('InputOneValue,"com.siigna.module.base.create",inputRequest.get)
          else Start('InputOneValue,"com.siigna.module.base.create")
        } else if(inputType == Some(14) ) {
          if (drawGuide == true) drawGuide = false
          if (!inputRequest.isEmpty) Start('InputText,"com.siigna.module.base.create",inputRequest.get)
          else Start('InputText,"com.siigna.module.base.create")
        }
      }

      //Input received from other modules (eg. Input OneValue, InputTwoValues, InputText, AngleGizmo):

      //Vector2D: (Standard: The received Vector2D is returned, un-transformed)
      case End(p : Vector2D) :: tail => {
        if (drawGuide == false) drawGuide = true
        if (inputType == Some(102) || inputType == Some(1020) || inputType == Some(120)) {
          End(MouseUp(p,MouseButtonLeft,ModifierKeys(false,false,false)))
        } else if (inputType == Some(1021)) {
          End(MouseDown(p,MouseButtonLeft,ModifierKeys(false,false,false)))
        } else if (inputType == Some(112) && !referencePoint1.isEmpty) {
          End(referencePoint1.get + p)
        } else if (inputType == Some(17)) {
          End
        } else {
          End(p)
        }
      }
      //Double: (Standard: The received Double is returned)
      case End(s : Double) :: tail => {
        if (drawGuide == false) drawGuide = true
        if ((inputType == Some(16) || inputType == Some(111)|| inputType == Some(112)) && Track.isTracking == true) {
          End(Track.getPointFromDistance(s).get)
        } else {
          End(s)
        }
      }
      //String: (Standard: The received string is returned)
      case End(s : String) :: tail => {
        if (drawGuide == false) drawGuide = true
        End(s)
      }
      //MouseDown, left button(AngleGizmo does that): (Standard: The Vector2D returned with the mouseDown is returned, un-transformed)
      case End(MouseDown(p,MouseButtonLeft,modifier)) :: tail => {
        if (drawGuide == false) drawGuide = true
        End(p)
      }
      //MouseDown, right button: (Standard: The mouse action is returned)
      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End(MouseDown(p,MouseButtonRight,modifier))
      //Escape:
      case End(KeyDown(Key.escape,modifier)) :: tail => End(KeyDown(Key.escape,modifier))

      //Any other input: Standard: Nothing happens
      case _ => {
      }
    }
  )

  //Paint guides:
  override def paint(g : Graphics, t : TransformationMatrix) {
    if ((inputType == Some(12)  || inputType == Some(1020) || inputType == Some(120)) && turnGuideOn == false) drawGuide = false
    
    //draw the guide - but only if no points are being entered with keys, in which case the input modules are drawing.
    if ( drawGuide == true) {
      if (!vector2DGuide.isEmpty) vector2DGuide.get.vector2DGuide(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t)))
    }

    //Draw any Vector2DMessageGuides:
    if (!vector2DMessageGuide.isEmpty) {
      vector2DMessageGuide.get.vector2DMessageGuide(mousePosition)
    }
    
    if (!doubleGuide.isEmpty && inputType == Some(120)){
      if (!referencePoint1.isEmpty && !mouseDownPoint.isEmpty)
      doubleGuide.get.doubleGuide(-((mousePosition.transform(View.deviceTransformation) - referencePoint1.get).angle - (mouseDownPoint.get.transform(View.deviceTransformation) - referencePoint1.get).angle)).foreach(s => g.draw(s.transform(t)))
    }
  }
}

/** InputType descriptions:
 *
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
 *      Special guide:                Do not draw Vector2DGuide
 * 13 = Double                        Key (one value)
 *      Vector2D                      MouseDown. Guide is drawn.
 * 14 = String                        Key input, text
 * 15 = Nothing                       Returns nothing from Input module. Can be used when
 * 16 = Vector2D                      Key input, one-coordinate, offset from existing point when on a track guide
 * 17 = Double                        Key - InputOneValue
 *      End                           All other inputs sends End (except escape and right-click, who send their standard (End(action)))
 *
 * 111 = Vector2D                  x  Point at mouseDown, or point by key(absolute - twoValues) or point guided by trackguide (input One value) if a track guide is active.
 * 112 = Vector2D                  x  Point at MouseDown, or vector2D by key added to referencePoint1 or point guided by trackguide (input One value) if a track guide is active.
 *
 * 102 =  mouseDown, with Vector2D    MouseDown (sent after mouseUp received)
 *        mouseUp, with Vector2D      coordinates from mouseDown to mouseUp, Key (absolute - handled by the InputTwoValues module)
 * 1020 = mouseDown, with Vector2D    MouseDown (sent after mouseUp received)
 *        mouseUp, with Vector2D      coordinates from mouseDown to mouseUp, Key (absolute - handled by the InputTwoValues module)
 *        Special guide:              Do not draw guide in input until left mouse button is clicked.
 * 1021 = mouseDown, with Vector2D    MouseDown (sent after mouseUp received)
 *        mouseUp, with Vector2D      coordinates from mouseDown to mouseUp, Key (absolute - handled by the InputTwoValues module)
 * 120 =  Vector2D                    MouseDown (sent after mouseUp received)
 *        Double                      Angle between legs og triangle between mouseDown-point, referencePoint1 and mouseUp-point.
 *        Double                      Key input
 *        Special guide:              Do not draw guide in input until left mouse button is clicked.
 *                                    Use doubleGuide in Input module.
 * 103 =  Double                      Length of vector from mouseDown to mouseUp, or key-input
 *        mouseDown, with Vector2D    mouseDown (sent after mouseUp received)
 * 1031 = Double                      key-input
 *        Vector2D                    Point at mouseDown
 * 
 *
 * General functioning of:
 *   Right button: Depends on the way it is natural for the module to function - priorotised uses:
 *     -Finish the modules' work (eg. polyline)
 *     -Exit the module (eg. line)
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
