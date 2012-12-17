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
import scala.Predef._

/**
 * An object that handles the angle-gizmo.
 */

class AngleGizmo extends Module {

  //variables:
  var currentSnap : Option[AngleSnap] = None
  var anglePointIsSet = false    // a flag telling if the desired angle is set
  var ctrl = false    //a flag to disregard the timer if CTRL is pressed
  private var degrees : Option[Double] = None   //The degree of the angle-guide, given in degrees where 0 is North clockwise.
  var guideLength = 0
  var gizmoMode = 45
  val gizmoRadius = 220
  val gizmoScale = 0.7
  val gizmoShapes = List[Shape]()
  val gizmoTime = 300   //time to press and hold the mouse button before the gizmo mode is activated
  private var drawGizmo = true
  private var drawGuide = true
  private var backFromOneValue = false
  private var oldMousePosition: Option[Vector2D] = None

  private var inputType : Option[Int] = None

  private var point1 : Option[Vector2D] = None
  private var point2 : Option[Vector2D] = None
  
  var pointGuide : Option[Vector2D => Traversable[Shape]] = None
  var doubleGuide : Option[Double => Traversable[Shape]] = None

  var sendPointGuide : Option[PointGuide] = None
  var sendDoubleGuide : Option[DoubleGuide] = None
  var sendPointPointGuide : Option[PointPointGuide] = None
  var sendPointDoubleGuide : Option[PointDoubleGuide] = None
  var sendPointPointDoubleGuide : Option[PointPointDoubleGuide] = None
  var sendPointPointPointGuide : Option[PointPointPointGuide] = None
  

  //TODO: implement activation of Angle Gizmo if left mouse is pressed for 1-2 sec while in the Input module.
  //private var startTime : Option[Long] = None

  //a function to add a typed distance to a line, after the Angle Gizmo has redined a radial.
  def lengthVector(length : Double) : Vector2D = {
    //a vector that equals the length of the typed distance, rotated by the current radial snap setting.
    var rotatedVector = Vector2D(math.sin(currentSnap.get.degree * math.Pi/180), math.cos(currentSnap.get.degree * math.Pi/180)) * length
    //and transformed by the center point of the offset from the Angle snap gizmo.
    rotatedVector + currentSnap.get.center
  }

  // Snaps an angle to the current interval of the gizmo mode.
  def roundSnap(angle : Double) = ((angle/gizmoMode).round * gizmoMode).round.toInt

  val stateMap: StateMap = Map(
    'Start -> {

      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      case Start(_ ,g : PointGuide) :: tail => {
        pointGuide = Some(g.pointGuide)
        inputType = Some(g.inputType)
        sendPointGuide = Some(g)
      }

      case Start(_ ,g : DoubleGuide) :: tail => {
        doubleGuide = Some(g.doubleGuide)
        inputType = Some(g.inputType)
        sendDoubleGuide = Some(g)
      }

      case Start(_ ,g : PointPointGuide) :: tail => {
        pointGuide = Some(g.pointGuide)
        inputType = Some(g.inputType)
        sendPointPointGuide = Some(g)
        point1 = Some(g.point1)
        eventParser.snapTo(() => g.pointGuide(mousePosition))
      }

      case Start(_ ,g : PointDoubleGuide) :: tail => {
        doubleGuide = Some(g.doubleGuide)
        inputType = Some(g.inputType)
        sendPointDoubleGuide = Some(g)
        point1 = Some(g.point1)
      }

      case Start(_ ,g : PointPointDoubleGuide) :: tail => {
        doubleGuide = Some(g.doubleGuide)
        inputType = Some(g.inputType)
        sendPointPointDoubleGuide = Some(g)
        point1 = Some(g.point1)
        point2 = Some(g.point2)
      }

      case Start(_ ,g : PointPointPointGuide) :: tail => {
        pointGuide = Some(g.pointGuide)
        inputType = Some(g.inputType)
        sendPointPointPointGuide = Some(g)
        point1 = Some(g.point1)
        point2 = Some(g.point2)
      }

      //If there is no guide, only the input type needs to be retrieved
      case Start(_,inp: Int) :: tail => {
        inputType = Some(inp)
      }  

      case MouseMove(p, _, _) :: tail => {
        Siigna.navigation = false // Make sure the rest of the program doesn't move
        //get the current radial - but only if the angle is not set yet.
        if (point1.isDefined && !anglePointIsSet) {

          val m = mousePosition.transform(View.deviceTransformation)
          val clockwiseDegrees = (m - point1.get).angle.round.toInt * -1 // Flip the degree-value to get the clockwise values
          val northDegrees = (clockwiseDegrees + 360 + 90) % 360   // Move the 0 to North and normalize to [0; 360]

          // Save it
          degrees = Some(roundSnap(northDegrees))
          //if the radial is set, calculate the length of the guide from the point1 to the mousePosition
        }
      }

      //if the right mouse button is pressed, exit.
      case (MouseUp(_, MouseButtonRight, _) | MouseDown(_, MouseButtonRight, _)) :: tail => End

      //case MouseUp(_, _, _) :: MouseDrag(_, _, _) :: tail => {
      //  anglePointIsSet = true
      //  End
      //}

      // if the left mouse button is pressed (after the mouse has been moved), then set the radial.
      case MouseDown(p, button, modifier) :: MouseMove(_, _, _) :: tail =>  {
        //return the angle
        if (point1.isDefined && degrees.isDefined && anglePointIsSet == false) {
          //send the active snap angle
          val point = point1.get
          val d = degrees.get
          anglePointIsSet = true
          currentSnap = Some(new AngleSnap(point,d))
          eventParser.snapTo(currentSnap.get)
          drawGizmo = false
        } else if (anglePointIsSet) {
          End(MouseDown(p.transform(View.deviceTransformation),button,modifier))
        }
      }

      case KeyDown(key,modifier) :: tail => {
        Siigna.navigation = false // Make sure the rest of the program doesn't move
        drawGuide = false
        var guide: Option[DoubleGuide] = None
        //A DoubleGuide for a line is sent to InputOneValue, to draw a guide for the segment being drawn:
        if (anglePointIsSet == false) {
          guide = Some(DoubleGuide((d: Double) => Traversable(LineShape(point1.get, point1.get + (Vector2D(math.sin(d * math.Pi/180), math.cos(d * math.Pi/180)) * 150))),15))
        } else {
          guide = Some(DoubleGuide((d: Double) => Traversable(LineShape(point1.get, lengthVector (d))),15))
        }
        Start('InputOneValue,"com.siigna.module.base.create", guide.get)
      }

      case End(d : Double) :: tail => {
        if (anglePointIsSet == false) {
        val point = point1.get
        currentSnap = Some(new AngleSnap(point,d))
        anglePointIsSet = true
        eventParser.snapTo(currentSnap.get)
        degrees = Some(d)
        drawGizmo = false
        drawGuide = true
        backFromOneValue = true
        Siigna.navigation = true

        } else {
          Siigna.navigation = true
          End(MouseDown(lengthVector(d),MouseButtonLeft,ModifierKeys(false,false,false)))
        }
      }

      //case KeyUp(Key.Control, _) :: tail => {
      //  Goto('End, false)
      //}

      case x=> println(x)
    }
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    //TODO: forward and draw shapes to the Angle gizmo, and draw them dynamically while defining the angle.
    //get the point Guide from the calling module:

    //if (point1.isDefined && (startTime.isDefined && System.currentTimeMillis() - startTime.get > gizmoTime)) {
    if (point1.isDefined && anglePointIsSet == false  && drawGizmo == true) {
      //If there is no ongoing key-input, draw the whole guide:
      if (!pointGuide.isEmpty && drawGuide == true) {
        pointGuide.foreach(_(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t))))
        //If there is key-input, only draw the fixed part of the shape - the last part being created is drawn by InputOneValue:
      } else if (!pointGuide.isEmpty && drawGuide == false) {
        pointGuide.foreach(_(point1.get).foreach(s => g.draw(s.transform(t))))
      }

      var m = mousePosition.transform(View.deviceTransformation)

      //modify the TransformationMatrix to preserve AngleGizmo scaling.
      def scaling(a : Double) = scala.math.pow(a,-1)

      val transformation : TransformationMatrix = t.scale((scaling(View.zoom)*gizmoScale), point1.get)

      //If there is text-input (drawGuide == false), the gizmo-modes are not needed, and the text doesn't need to be drawn:
      if (drawGuide == true) {
        //Set Angle Gizmo mode based on distance to center
        def distanceToStart = m - point1.get
        if (distanceToStart.length < 50*scaling(View.zoom)*gizmoScale) gizmoMode = 90
        else if (distanceToStart.length > 50*scaling(View.zoom)*gizmoScale && distanceToStart.length < 100*scaling(View.zoom)*gizmoScale) gizmoMode = 45
        else if (distanceToStart.length > 100*scaling(View.zoom)*gizmoScale && distanceToStart.length < 170*scaling(View.zoom)*gizmoScale) gizmoMode = 10
        else if (distanceToStart.length > 170*scaling(View.zoom)*gizmoScale && distanceToStart.length < 200*scaling(View.zoom)*gizmoScale) gizmoMode = 5
        else gizmoMode = 1

        if (gizmoMode == 1) {guideLength = 195 }
        else if (gizmoMode == 5) {guideLength = 165 }
        else if (gizmoMode == 10) {guideLength = 95 }
        else guideLength = 45

        // Draw the text
        if (degrees.isDefined) {
          g draw TextShape((roundSnap(degrees.get)).toString, Vector2D(point1.get.x, point1.get.y + 240).transform(transformation.rotate(roundSnap(-degrees.get), point1.get)), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector2D(0.5,0.5)))
        }
      }

      //draw inactive Angle Gizmo shapes
      def getLine(d1 : Int, d2 : Int, mode : Int) = LineShape(Vector2D(point1.get.x, point1.get.y + d1), Vector2D(point1.get.x, point1.get.y + d2), Attributes("Color" -> (if (gizmoMode == mode) "#999999" else "#CDCDCD").color))

      // Draw the radians
      (0 to 360 by 45).foreach(radian => g draw getLine(50, 100, 45).transform(transformation.rotate(radian, point1.get)))
      (0 to 360 by 10).foreach(radian => g draw getLine(100, 170, 10).transform(transformation.rotate(radian, point1.get)))
      (0 to 360 by 5).foreach(radian => g draw getLine(170, 200, 5).transform(transformation.rotate(radian, point1.get)))
      (0 to 360 by 1).foreach(radian => g draw getLine(200, 220, 1).transform(transformation.rotate(radian, point1.get)))

      //If anglePointSet is true, the angle has been set, and length is the only thing left.
      // There is no need to display the angle. Draw the guide:
    } else if (point1.isDefined && anglePointIsSet == true) {
      //If there is no key-input, draw the whole guide:
        //If the it is just after the angle was set, dont use the mouse position to draw the guide,
        //as it is not on the correct radian. Use point based on lengthVector instead, until the mouse is moved:
      if (!pointGuide.isEmpty && drawGuide == true && backFromOneValue == true) {        
        pointGuide.foreach(_(lengthVector(150)).foreach(s => g.draw(s.transform(t))))
        if (oldMousePosition.isEmpty) oldMousePosition = Some(mousePosition)
        if (mousePosition != oldMousePosition.get) backFromOneValue = false
      } else if (!pointGuide.isEmpty && drawGuide == true) {
        pointGuide.foreach(_(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t))))
        //If there is key-input, only draw the fixed part of the shape - the last part being created is drawn by InputOneValue
      } else if (!pointGuide.isEmpty && drawGuide == false) {
        pointGuide.foreach(_(point1.get).foreach(s => g.draw(s.transform(t))))
      }
    }
  }
}
