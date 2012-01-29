package com.siigna.module.endogenous.create

import com.siigna._

/**
 * An object that handles the angle-gizmo.
 */
//TODO: multiply with zoomscale to get a fixed size gizmo

object AngleGizmo extends Module {

  // a flag telling if the desired angle is set
  var anglePointIsSet = false

  /**
   * The degree of the angle-guide, given in degrees where 0 is North clockwise.
   */
  private var degrees : Option[Int] = None

  var guideLength = 0
  var gizmoMode = 45
  val gizmoRadius = 220
  val gizmoShapes = List[Shape]()

  //time to press and hold the mouse button before the gizmo mode is activated
  val gizmoTime = 700
  
  // A flag to determine whether the angle gizmo was activated
  private var gizmoIsActive = false

  // var to check if the Angle Gizmo is running. Can be used by modules to change what is drawn when the gizmo is active
  var inAngleGizmoMode = false

  /**
   * The latest event the module received.
   */
  var latestEvent : Option[Event] = None

  // The starting point of the angle gizmo
  var startPoint : Option[Vector2D] = None

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start         -> 'KeyEscape -> 'End,
    'Mousecheck    -> 'KeyEscape -> 'End,
    'AngleGizmo    -> 'KeyEscape -> 'End
  )

  def stateMachine = Map(
    //arriving in GIZMO with latest event: MouseDown
    'Start -> ((events : List[Event]) => {
      // Start the loop
      val a = new AngleGizmoLoop
      a.start()

      // Listen to mouse-events
      Goto('MouseCheck)
    }),
    //check if a mouse up is happening while running the angle gizmo loop, if so, the angle module will exit.
    'MouseCheck -> ((events : List[Event]) => {

      // Store the latest event
      latestEvent = Some(events.head)

      //if these movements are registered while the AngleGizmoLoop is running, stay in Mouse Check
      events match {
        //if the latest event is still MouseDown, stay.
        case MouseDown(p, MouseButtonLeft, _) :: tail => startPoint = Some(p)
        case MouseDrag(p, _, _) :: tail               => startPoint = Some(p)
        //if anything else is received, end the gizmo without any angle guide
        case _ => {
          if (startPoint.isEmpty)
            startPoint = Some(Siigna.mousePosition)

          Goto('End)
          //send a message that tell no angle is given
        }
      }
    }),
    //this state is activated if the Gizmo is called:
    'AngleGizmo -> ((events : List[Event]) => {
      // Activate!
      gizmoIsActive = true
      events match {
        //if the right mouse button is pressed, exit.
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        //if the mouse is clicked, go to 'End, and do not return the latest event
        case MouseDown(_, MouseButtonRight, _) :: tail => Goto('End, false)
        //the latest event coming from polyline has to be mouse down, so
        // this event forms the basis for getting the current mouse position:

        case MouseMove(p ,_ ,_) :: tail =>

        case MouseDown(p, MouseButtonLeft, _) :: MouseMove(_, _, _) :: tail =>  {
          if(anglePointIsSet == true)
            Goto('End, false)
          else anglePointIsSet = true

        }
        case _=>
      }
      //get the current radial
      if (startPoint.isDefined) {
        // Flip the degree-value to get the clockwise values
        val clockwiseDegrees = (Siigna.mousePosition - startPoint.get).angle.round.toInt * -1

        // Move the 0 to North and normalize to [0; 360]
        val northDegrees = (clockwiseDegrees + 360 + 90) % 360

        // Save it
        degrees = Some(northDegrees)
      }
    }),
    //return the output of the anonymous function f, declared above the StateMachine
    'End -> ((events : List[Event]) => {
      // If the gizmo was activated, then return the message and reset the vars
      if (gizmoIsActive) {
        //reset the flag
        gizmoIsActive = false
        if (anglePointIsSet == true && startPoint.isDefined && degrees.isDefined) {
          //send the active snap angle
          Send(Message(new AngleSnap(startPoint.get, degrees.get)))
          degrees = None
        }
      }
        //if the gizmo is not needed, but a point has been set, return the point in a message, but send no angle.
      else if (startPoint.isDefined) {
        gizmoIsActive = false
        degrees = None
        // If the gizmo was not activated, then return the point so the
        // point module can utilize it to whatever
        Send(Message(startPoint.get))
        //forward to the point module, leaving the last event (mouseUp) out, as it would exit the Point module.
        ForwardTo('Point, false)
        }
      else {
      }
      // Reset variables
      anglePointIsSet = false
      startPoint = None
    })
  )

  //Draw the Angle Gizmo perimeter
  override def paint(g : Graphics, t : TransformationMatrix) {
    // Snaps an angle to the current interval of the gizmo mode.
    def roundSnap(angle : Double) = {
      ((angle)/gizmoMode).round*gizmoMode
    }

    if (startPoint.isDefined && degrees.isDefined && gizmoIsActive) {
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
      g draw TextShape((roundSnap(degrees.get)).toString, Vector2D(startPoint.get.x, startPoint.get.y + 240).transform(t.rotate(roundSnap(-degrees.get), startPoint.get)), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector2D(0.5,0.5)))
      g draw LineShape(startPoint.get,Vector2D(startPoint.get.x, startPoint.get.y+guideLength)).transform(t.rotate(-roundSnap(degrees.get), startPoint.get))
    }
  }
}