/*
 * Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.cad.create

import com.siigna._
import scala.Predef._
import java.awt.Color

/**
 * An object that handles the angle-gizmo.
 */

class AngleGizmo extends Module {

  //variables:
  var currentSnap : Option[AngleSnap] = None
  var anglePointIsSet = false    // a flag telling if the desired angle is set
  var ctrl = false    //a flag to disregard the timer if CTRL is pressed
  private val cyan = "Color" -> new Color(0.10f, 0.95f, 0.95f, 1.00f)
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


  var inputRequest: Option[InputRequestNew] = None
  var inputType: Option[Int] = None
  var guides: Seq[Guide] = Seq()
  var referencePoint: Option[Vector2D] = None


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

      //Check for input request:
      case Start(_ , i: InputRequestNew) :: tail => {
        inputRequest = Some(i)
        inputType = Some(i.inputType)
        guides = i.guides
        referencePoint = i.referencePoint

        if (referencePoint.isEmpty && Track.isTracking == true) {
          referencePoint = Track.pointOne
          val newGuides : Seq[Guide] = inputRequest.get.guides.:+(Vector2DGuideNew((v: Vector2D) => Traversable(LineShape(referencePoint.get, v))))
          inputRequest = Some(InputRequestNew(inputRequest.get.inputType, inputRequest.get.referencePoint, newGuides:_*))
        }

        //TODO: Make line to the start-point of a new shape dashed instead of solid
      }


      case MouseMove(p, _, _) :: tail => {
        if (backFromOneValue == true) backFromOneValue = false

        Siigna.navigation = false // Make sure the rest of the program doesn't move
        //get the current radial - but only if the angle is not set yet.
        if (referencePoint.isDefined && !anglePointIsSet) {

          val m = mousePosition.transform(View.deviceTransformation)
          val clockwiseDegrees = (m - referencePoint.get).angle.round.toInt * -1 // Flip the degree-value to get the clockwise values
          val northDegrees = (clockwiseDegrees + 360 + 90) % 360   // Move the 0 to North and normalize to [0; 360]

          // Save it
          degrees = Some(roundSnap(northDegrees))
          //if the radial is set, calculate the length of the guide from the referencePoint1 to the mousePosition
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
        if (referencePoint.isDefined && degrees.isDefined && anglePointIsSet == false) {
          //send the active snap angle
          backFromOneValue = true
          val point = referencePoint.get
          val d = degrees.get
          anglePointIsSet = true
          currentSnap = Some(new AngleSnap(point,d))
          eventParser.snapTo(currentSnap.get)
          drawGizmo = false
        } else if (anglePointIsSet) {
          Siigna.navigation = true
          End(MouseDown(p.transform(View.deviceTransformation),button,modifier))
        }
      }

      case KeyDown(key,modifier) :: tail => {
        if (backFromOneValue == true) backFromOneValue = false

        Siigna.navigation = false // Make sure the rest of the program doesn't move
        drawGuide = false
        //A DoubleGuide for a line is sent to InputOneValue, to draw a guide for the segment being drawn:
        if (anglePointIsSet == false) {
          val newGuides : Seq[Guide] = inputRequest.get.guides.:+(Vector2DGuideNew((v: Vector2D) => Traversable(LineShape(referencePoint.get, v))))
          inputRequest = Some(InputRequestNew(inputRequest.get.inputType, inputRequest.get.referencePoint, newGuides:_*))


          doubleGuide = Some(DoubleGuide((d: Double) => Traversable(LineShape(referencePoint.get, referencePoint.get + (Vector2D(math.sin(d * math.Pi/180), math.cos(d * math.Pi/180)) * referencePoint.get.distanceTo(mousePosition.transform(View.deviceTransformation)))).addAttribute(cyan))))
        } else {
          doubleGuide = Some(DoubleGuide((d: Double) => vector2DGuide.get.vector2DGuide(lengthVector(d))))
        }
        val referenceDouble = referencePoint.get.distanceTo(mousePosition.transform(View.deviceTransformation))
        //val inputRequest = InputRequest(None,doubleGuide,None,None,None,None,None,None,Some(referenceDouble),Some(15))
        Start('cad,"create.InputOneValue", inputRequest)
      }

      case End(d : Double) :: tail => {
        if (anglePointIsSet == false) {
        val point = referencePoint.get
        currentSnap = Some(new AngleSnap(point,d))
        anglePointIsSet = true
        eventParser.snapTo(currentSnap.get)
        degrees = Some(d)
        drawGizmo = false
        drawGuide = true
        backFromOneValue = true

        } else {
          Siigna.navigation = true
          End(MouseDown(lengthVector(d),MouseButtonLeft,ModifierKeys(false,false,false)))
        }
      }

      //case KeyUp(Key.Control, _) :: tail => {
      //  Goto('End, false)
      //}

      case x=> println("AG: " + x)
    }
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    //TODO: forward and draw shapes to the Angle gizmo, and draw them dynamically while defining the angle.
    //get the point Guide from the calling module:

    //if (referencePoint1.isDefined && (startTime.isDefined && System.currentTimeMillis() - startTime.get > gizmoTime)) {
    if (referencePoint.isDefined && anglePointIsSet == false  && drawGizmo == true) {

      var m = mousePosition.transform(View.deviceTransformation)

      //modify the TransformationMatrix to preserve AngleGizmo scaling.
      def scaling(a : Double) = scala.math.pow(a,-1)

      val transformation : TransformationMatrix = t.scale((scaling(View.zoom)*gizmoScale), referencePoint.get)

      //If there is text-input (drawGuide == false), the gizmo-modes are not needed, and the text doesn't need to be drawn:
      if (drawGuide == true) {
        //Set Angle Gizmo mode based on distance to center
        def distanceToStart = m - referencePoint.get
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
          g draw TextShape((roundSnap(degrees.get)).toString, Vector2D(referencePoint.get.x, referencePoint.get.y + 240).transform(transformation.rotate(roundSnap(-degrees.get), referencePoint.get)), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector2D(0.5,0.5)))
        }
      }

      //draw inactive Angle Gizmo shapes
      def getLine(d1 : Int, d2 : Int, mode : Int) = LineShape(Vector2D(referencePoint.get.x, referencePoint.get.y + d1), Vector2D(referencePoint.get.x, referencePoint.get.y + d2), Attributes("Color" -> (if (gizmoMode == mode) "#999999" else "#CDCDCD").color))

      // Draw the radians
      (0 to 360 by 45).foreach(radian => g draw getLine(50, 100, 45).transform(transformation.rotate(radian, referencePoint.get)))
      (0 to 360 by 10).foreach(radian => g draw getLine(100, 170, 10).transform(transformation.rotate(radian, referencePoint.get)))
      (0 to 360 by 5).foreach(radian => g draw getLine(170, 200, 5).transform(transformation.rotate(radian, referencePoint.get)))
      (0 to 360 by 1).foreach(radian => g draw getLine(200, 220, 1).transform(transformation.rotate(radian, referencePoint.get)))

      //Mouse position snapped to current angle-interval:
      var angleSnappedMousePosition: Option[Vector2D] = None
      if(!degrees.isEmpty) {
        val distance: Double = referencePoint.get.distanceTo(mousePosition.transform(View.deviceTransformation))
        angleSnappedMousePosition = Some(referencePoint.get + Vector2D(math.sin(degrees.get.toRadians),math.cos(degrees.get.toRadians))*distance)
      } else angleSnappedMousePosition = Some(mousePosition.transform(View.deviceTransformation))
      
      //If there is no ongoing key-input, draw the whole guide:
      if (!vector2DGuide.isEmpty && drawGuide == true) {
        //ny farve
        vector2DGuide.get.vector2DGuide(angleSnappedMousePosition.get).foreach(s => g.draw(s.transform(t)))
        //If there is key-input, only draw the fixed part of the shape - the last part being created is drawn by InputOneValue:
      } else if (!vector2DGuide.isEmpty && drawGuide == false) {
        //vector2DGuide.get.vector2DGuide(referencePoint1.get).foreach(s => g.draw(s.transform(t)))
      }

      //If anglePointSet is true, the angle has been set, and length is the only thing left.
      // There is no need to display the angle. Draw the guide:
    } else if (referencePoint.isDefined && anglePointIsSet == true) {
      //If there is no key-input, draw the whole guide:
        //If the it is just after the angle was set, dont use the mouse position to draw the guide,
        //as it is not on the correct radian. Use point based on lengthVector instead, until the mouse is moved:
      if (!vector2DGuide.isEmpty && drawGuide == true && backFromOneValue == true) {
        //ny farve
        vector2DGuide.get.vector2DGuide(lengthVector(referencePoint.get.distanceTo(mousePosition.transform(View.deviceTransformation)))).foreach(s => g.draw(s.transform(t)))
      } else if (!vector2DGuide.isEmpty && drawGuide == true) {
        //ny farve her:
        vector2DGuide.get.vector2DGuide(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t)))
        //If there is key-input, only draw the fixed part of the shape - the last part being created is drawn by InputOneValue
      } else if (!vector2DGuide.isEmpty && drawGuide == false) {
        vector2DGuide.get.vector2DGuide(referencePoint.get).foreach(s => g.draw(s.transform(t)))
      }
    }
  }
}
