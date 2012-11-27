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
  var anglePointIsSet = false    // a flag telling if the desired angle is set
  var ctrl = false    //a flag to disregard the timer if CTRL is pressed
  private var degrees : Option[Int] = None   //The degree of the angle-guide, given in degrees where 0 is North clockwise.
  var guideLength = 0
  var gizmoMode = 45
  val gizmoRadius = 220
  val gizmoScale = 0.7
  val gizmoShapes = List[Shape]()
  val gizmoTime = 300   //time to press and hold the mouse button before the gizmo mode is activated
  private var gizmoIsActive = false     // A flag to determine whether the angle gizmo was activated
  var pointGuide : Option[Vector2D => Traversable[Shape]] = None
  var pointPointGuide : Option[Vector2D => Traversable[Shape]] = None
  private var startPoint : Option[Vector2D] = None    // The starting point of the angle gizmo

  //TODO: implement activation of Angle Gizmo if left mouse is pressed for 1-2 sec while in the Input module.
  //private var startTime : Option[Long] = None

  // Snaps an angle to the current interval of the gizmo mode.
  def roundSnap(angle : Double) = ((angle/gizmoMode).round * gizmoMode).round.toInt

  val stateMap: StateMap = Map(
    'Start -> {

      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      case Start(_ ,g : PointGuide) :: tail => {
        pointGuide = Some(g.pointGuide)
      }

      case Start(_ ,g : PointPointGuide) :: tail => {
        pointPointGuide = Some(g.pointGuide)
        startPoint = Some(g.point1)
      }

      case MouseMove(p, _, _) :: tail => {
        //get the current radial - but only if the angle is not set yet.
        if (startPoint.isDefined && !anglePointIsSet) {

          val m = mousePosition.transform(View.deviceTransformation)
          val clockwiseDegrees = (m - startPoint.get).angle.round.toInt * -1 // Flip the degree-value to get the clockwise values
          val northDegrees = (clockwiseDegrees + 360 + 90) % 360   // Move the 0 to North and normalize to [0; 360]

          // Save it
          degrees = Some(roundSnap(northDegrees))
          //if the radial is set, calculate the length of the guide from the startPoint to the mousePosition
        }
      }

      //if the right mouse button is pressed, exit.
      case (MouseUp(_, MouseButtonRight, _) | MouseDown(_, MouseButtonRight, _)) :: tail => End

      //case MouseUp(_, _, _) :: MouseDrag(_, _, _) :: tail => {
      //  anglePointIsSet = true
      //  End
      //}

      // if the left mouse button is pressed (after the mouse has been moved), then set the radial.
      case MouseDown(p, _, _) :: MouseMove(_, _, _) :: tail =>  {
        //return the angle
        if (startPoint.isDefined && degrees.isDefined) {
          //send the active snap angle
          val point = startPoint.get
          val d = degrees.get
          println("startPoint AG: "+point)
          End(new AngleSnap(point, d))
        }
      }
      //case KeyUp(Key.Control, _) :: tail => {
      //  Goto('End, false)
      //}

      case _=>
    }
  )
  override def paint(g : Graphics, t : TransformationMatrix) {

    //TODO: forward and draw shapes to the Angle gizmo, and draw them dynamically while defining the angle.
    //get the point Guide from the calling module:

    //if (startPoint.isDefined && (startTime.isDefined && System.currentTimeMillis() - startTime.get > gizmoTime)) {
    if (startPoint.isDefined) {

      var m = mousePosition.transform(View.deviceTransformation)

      //draw the shape under creation:
      var parsedPoint = Vector2D(startPoint.get.x, startPoint.get.y+guideLength)
      //using startPoint is a hack to prevent the line from the last point to the mousePosition being drawn on top of the angle guide.
      //TODO: use a guide that allows rectangles to be drawn dynamically ( mousePosition works, but is not snapped to the radians.) parsedPoint does not work??
      //if(pointGuide.isDefined) guide(startPoint.get).foreach(s => g draw s.transform(t))
      //if (!pointGuide.isEmpty && !startPoint.isEmpty ) pointGuide.foreach(_(Vector2D((mousePosition - startPoint.get).x,-(mousePosition - startPoint.get).y)).foreach(s => g.draw(s.transform(t))))


      //modify the TransformationMatrix to preserve AngleGizmo scaling.
      def scaling(a : Double) = scala.math.pow(a,-1)

      val transformation : TransformationMatrix = t.scale((scaling(View.zoom)*gizmoScale), startPoint.get)

      //Set Angle Gizmo mode based on distance to center
      def distanceToStart = m - startPoint.get
      if (distanceToStart.length < 50*scaling(View.zoom)*gizmoScale) gizmoMode = 90
      else if (distanceToStart.length > 50*scaling(View.zoom)*gizmoScale && distanceToStart.length < 100*scaling(View.zoom)*gizmoScale) gizmoMode = 45
      else if (distanceToStart.length > 100*scaling(View.zoom)*gizmoScale && distanceToStart.length < 170*scaling(View.zoom)*gizmoScale) gizmoMode = 10
      else if (distanceToStart.length > 170*scaling(View.zoom)*gizmoScale && distanceToStart.length < 200*scaling(View.zoom)*gizmoScale) gizmoMode = 5
      else gizmoMode = 1

      if (gizmoMode == 1) {guideLength = 195 }
      else if (gizmoMode == 5) {guideLength = 165 }
      else if (gizmoMode == 10) {guideLength = 95 }
      else guideLength = 45

      //draw inactive Angle Gizmo shapes
      def getLine(d1 : Int, d2 : Int, mode : Int) = LineShape(Vector2D(startPoint.get.x, startPoint.get.y + d1), Vector2D(startPoint.get.x, startPoint.get.y + d2), Attributes("Color" -> (if (gizmoMode == mode) "#999999" else "#CDCDCD").color))

      // Draw the radians
      (0 to 360 by 45).foreach(radian => g draw getLine(50, 100, 45).transform(transformation.rotate(radian, startPoint.get)))
      (0 to 360 by 10).foreach(radian => g draw getLine(100, 170, 10).transform(transformation.rotate(radian, startPoint.get)))
      (0 to 360 by 5).foreach(radian => g draw getLine(170, 200, 5).transform(transformation.rotate(radian, startPoint.get)))
      (0 to 360 by 1).foreach(radian => g draw getLine(200, 220, 1).transform(transformation.rotate(radian, startPoint.get)))

      // Draw the text and the active angle
      if (degrees.isDefined) {
        g draw TextShape((roundSnap(degrees.get)).toString, Vector2D(startPoint.get.x, startPoint.get.y + 240).transform(transformation.rotate(roundSnap(-degrees.get), startPoint.get)), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector2D(0.5,0.5)))
        g draw LineShape(startPoint.get,parsedPoint).transform(transformation.rotate(-roundSnap(degrees.get), startPoint.get))
      }
    }
  }
}