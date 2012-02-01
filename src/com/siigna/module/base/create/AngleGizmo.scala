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
 * An object that handles the angle-gizmo.
 */
//TODO: multiply with zoomscale to get a fixed size gizmo

object AngleGizmo extends Module {

  // a flag telling if the desired angle is set
  var anglePointIsSet = false

  //a flag to disregard the timer if CTRL is pressed
  var ctrl = false

  /**
   * The degree of the angle-guide, given in degrees where 0 is North clockwise.
   */
  private var degrees : Option[Int] = None

  var guideLength = 0
  var gizmoMode = 45
  val gizmoRadius = 220
  val gizmoShapes = List[Shape]()

  //time to press and hold the mouse button before the gizmo mode is activated
  val gizmoTime = 300
  
  // A flag to determine whether the angle gizmo was activated
  private var gizmoIsActive = false

  // The starting point of the angle gizmo
  private var startPoint : Option[Vector2D] = None

  private var startTime : Option[Long] = None

  def eventHandler = EventHandler(stateMap, stateMachine)

  // Snaps an angle to the current interval of the gizmo mode.
  def roundSnap(angle : Double) = ((angle/gizmoMode).round * gizmoMode).round.toInt

  def stateMap = DirectedGraph(
    'Start         -> 'KeyEscape -> 'End,
    'Mousecheck    -> 'KeyEscape -> 'End,
    'AngleGizmo    -> 'KeyEscape -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      startTime = Some(System.currentTimeMillis())

      events match {
        case Message(_) :: Message(_) :: MouseDown(p, _, _) :: tail => {
          startPoint = Some(p)
        }
        case Message(p : Vector2D) :: KeyDown(Key.Shift, _) :: tail => {
          startPoint = Some(p)
        }
        case _ =>
      }
      // Listen to mouse-events
      Goto('MouseCheck, false)
    }),
    //check if a mouse up is happening while running the angle gizmo loop, if so, the angle module will exit.
    'MouseCheck -> ((events : List[Event]) => {
      //if CTRL was presed in point, activate the gizmo
      events match {
        case KeyUp(Key.Shift, _) :: Message(_) :: tail => {
          ctrl = true
          Goto('AngleGizmo)
        }

        case _ =>
      }
      // If the gizmo check has expired then activate the gizmo,
      // if the latest event has not been set before
      if (!ctrl == true && System.currentTimeMillis() - startTime.get < gizmoTime) {
        Goto('End)

      // If the time has run out and an event has been registered, then exit
      } else {
        Goto('AngleGizmo)

      // Otherwise continue
      }
    }),
    //this state is activated if the Gizmo is called:
    'AngleGizmo -> ((events : List[Event]) => {
      // Activate!
      gizmoIsActive = true
      events match {
        //if the right mouse button is pressed, exit.
        case (MouseUp(_, MouseButtonRight, _) | MouseDown(_, MouseButtonRight, _)) :: tail => {
          Goto('End, false)
        }

        case MouseUp(_, _, _) :: MouseDrag(_, _, _) :: tail => {
          anglePointIsSet = true
          Goto('End)
        }

        case MouseDown(p, _, _) :: MouseMove(_, _, _) :: tail =>  {
          anglePointIsSet = true
          Goto('End)
        }
        //case KeyUp(Key.Control, _) :: tail => {
        //  Goto('End, false)
        //}

        case _=>

      }

      //get the current radial
      if (startPoint.isDefined) {
        // Flip the degree-value to get the clockwise values
        val clockwiseDegrees = (Siigna.mousePosition - startPoint.get).angle.round.toInt * -1

        // Move the 0 to North and normalize to [0; 360]
        val northDegrees = (clockwiseDegrees + 360 + 90) % 360

        // Save it
        degrees = Some(roundSnap(northDegrees))
      }
    }),
    'End -> ((events : List[Event]) => {
      def reset() {
        //Reset variables
        ctrl = false
        degrees = None
        gizmoIsActive = false
        anglePointIsSet = false
        startPoint = None
        startTime = None
      }

      // If the gizmo was activated, then return the message and reset the vars
      if (gizmoIsActive && anglePointIsSet && startPoint.isDefined && degrees.isDefined) {
        //send the active snap angle
        val point = startPoint.get
        val d = degrees.get
        reset()
        Message(new AngleSnap(point, d))

      //if the gizmo is not needed, but a point has been set, return the point in a message, but send no angle.
      } else if (startPoint.isDefined) {
        val point = startPoint.get
        reset()
        // If the gizmo was not activated, then return the point so the
        // point module can utilize it to whatever
        Message(point)
      }
    })
  )

  //Draw the Angle Gizmo perimeter
  override def paint(g : Graphics, t : TransformationMatrix) {

    if (startPoint.isDefined && (startTime.isDefined && System.currentTimeMillis() - startTime.get > gizmoTime)) {
      //Set Angle Gizmo mode based on distance to center
      def distanceToStart = Siigna.mousePosition - startPoint.get
      if (distanceToStart.length < 50) gizmoMode = 90
      else if (distanceToStart.length > 50 && distanceToStart.length < 100) gizmoMode = 45
      else if (distanceToStart.length > 100 && distanceToStart.length < 170) gizmoMode = 10
      else if (distanceToStart.length > 170 && distanceToStart.length < 200) gizmoMode = 5
      else gizmoMode = 1

      if (gizmoMode == 1) {guideLength = 195 }
      else if (gizmoMode == 5) {guideLength = 165 }
      else if (gizmoMode == 10) {guideLength = 95 }
      else guideLength = 45

      //draw inactive Angle Gizmo shapes
      def getLine(d1 : Int, d2 : Int, mode : Int) = LineShape(Vector2D(startPoint.get.x, startPoint.get.y + d1), Vector2D(startPoint.get.x, startPoint.get.y + d2), Attributes("Color" -> (if (gizmoMode == mode) "#999999" else "#CDCDCD").color))

      // Draw the radians
      (0 to 360 by 45).foreach(radian => g draw getLine(50, 100, 45).transform(t.rotate(radian, startPoint.get)))
      (0 to 360 by 10).foreach(radian => g draw getLine(100, 170, 10).transform(t.rotate(radian, startPoint.get)))
      (0 to 360 by 5).foreach(radian => g draw getLine(170, 200, 5).transform(t.rotate(radian, startPoint.get)))
      (0 to 360 by 1).foreach(radian => g draw getLine(200, 220, 1).transform(t.rotate(radian, startPoint.get)))

      // Draw the text and the active angle
      if (degrees.isDefined) {
        g draw TextShape((roundSnap(degrees.get)).toString, Vector2D(startPoint.get.x, startPoint.get.y + 240).transform(t.rotate(roundSnap(-degrees.get), startPoint.get)), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector2D(0.5,0.5)))
        g draw LineShape(startPoint.get,Vector2D(startPoint.get.x, startPoint.get.y+guideLength)).transform(t.rotate(-roundSnap(degrees.get), startPoint.get))
      }
    }
  }
}
