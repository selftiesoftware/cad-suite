package com.siigna.module.endogenous.create

import com.siigna._

/**
 * An object that handles the angle-gizmo.
 */
//TODO: multiply with zoomscale to get a fixed size gizmo

object AngleGizmo extends Module {


  var activeAngle : Option[Double] = None

  // a flag telling if the desired angle is set
  var anglePointIsSet = false

  // var to check if the Angle Gizmo is running. Can be used by modules to change what is drawn when the gizmo is active
  var inAngleGizmoMode = false

  var guideLength = 0
  var gizmoMode = 45
  val gizmoRadius = 220
  val gizmoShapes = List[Shape]()

  //time to press and hold the mouse button before the gizmo mode is activated
  val gizmoTime = 700
  
  // A flag to determine whether the angle gizmo was activated
  private var gizmoIsActive = false

  /**
   * The latest event the module received.
   */
  var latestEvent : Option[Event] = None

  private var degrees : Option[Int] = None

  // The starting point of the angle gizmo
  var startPoint : Option[Vector2D] = None

  def round(angle : Double) = {
    ((angle)/gizmoMode).round*gizmoMode
  }
  def correct360(int : Int) = {
    if (int >= 360) int - 360 else int
  }

  //create a list including all radians in a given gizmo mode (45, 10 or 1 degrees)
  def radians(mode : Int) : List[Int] = {
    var i = 0
    var activeRadians = List[Int]()

    //add all radians to a list
    do {
      activeRadians = activeRadians :+ i
      i += mode

    } while (i <= 360)
    activeRadians
  }

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
        case MouseDrag(p, _, _) :: tail => {
          startPoint = Some(p)
        }
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
        degrees = Some((Siigna.mousePosition - startPoint.get).angle.toInt)
        var calculatedAngle = degrees.get * -1 + 450
        if (calculatedAngle > 360) {

          activeAngle = Some(calculatedAngle - 360)
        } else {
          activeAngle = Some(calculatedAngle)
        }
      }
    }),
    //return the output of the anonymous function f, declared above the StateMachine
    'End -> ((events : List[Event]) => {

      //convert degrees to radians
      def radians(d : Double) : Double = {(d.toInt * (scala.math.Pi / 180))}

      // If the gizmo was activated, then return the message and reset the vars
      if (gizmoIsActive) {
        //reset the flag
        gizmoIsActive = false
        if (anglePointIsSet == true && startPoint.isDefined && activeAngle.isDefined) {
          //send the active snap angle
          Send(Message(new AngleSnap(startPoint.get, radians(degrees.get))))
          activeAngle = None
        }
      }
        //if the gizmo is not needed, but a point has been set, return the point in a message, but send no angle.
      else if (startPoint.isDefined) {
        gizmoIsActive = false
        degrees = None
        activeAngle = None
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
    if (startPoint.isDefined && activeAngle.isDefined && gizmoIsActive) {
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

      val guide  = LineShape(startPoint.get,Vector2D(startPoint.get.x, startPoint.get.y+guideLength))

      //g draw CircleShape(startPoint.get, Vector2D(startPoint.get.x + gizmoRadius, startPoint.get.y)).transform(t)
      g draw TextShape((correct360(round(activeAngle.get).toInt)).toString, Vector2D(startPoint.get.x, startPoint.get.y + 240).transform(t.rotate(round(-activeAngle.get), startPoint.get)), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector2D(0.5,0.5)))
      g draw guide.transform(t.rotate((((round(activeAngle.get)* -1)+360).toInt), startPoint.get))

      //draw inactive Angle Gizmo shapes
      val inactive45 = LineShape(Vector2D(startPoint.get.x, startPoint.get.y+50), Vector2D(startPoint.get.x, startPoint.get.y+100), Attributes("Color" -> "#CDCDCD".color))
      val inactive10 = LineShape(Vector2D(startPoint.get.x, startPoint.get.y+100), Vector2D(startPoint.get.x, startPoint.get.y+170), Attributes("Color" -> "#CDCDCD".color))
      val inactive5  = LineShape(Vector2D(startPoint.get.x, startPoint.get.y+170), Vector2D(startPoint.get.x, startPoint.get.y+200), Attributes("Color" -> "#CDCDCD".color))
      val inactive1  = LineShape(Vector2D(startPoint.get.x, startPoint.get.y+200), Vector2D(startPoint.get.x, startPoint.get.y+220), Attributes("Color" -> "#CDCDCD".color))

      //TODO: why do these lines generate an error??!!
      radians(45).foreach(radian => {
        g draw inactive45.transform(t.rotate(radian, startPoint.get))
      })
      radians(10).foreach(radian => {
        g draw inactive10.transform(t.rotate(radian, startPoint.get))
      })
      radians(5).foreach(radian => {
        g draw inactive5.transform(t.rotate(radian, startPoint.get))
      })
      radians(1).foreach(radian => {
        g draw inactive1.transform(t.rotate(radian, startPoint.get))
      })

      //Draw the active Angle Gizmo shapes
      val line45 = LineShape(Vector2D(startPoint.get.x, startPoint.get.y+50), Vector2D(startPoint.get.x, startPoint.get.y+100), Attributes("Color" -> "#999999".color))
      val line10 = LineShape(Vector2D(startPoint.get.x, startPoint.get.y+100), Vector2D(startPoint.get.x, startPoint.get.y+170), Attributes("Color" -> "#999999".color))
      val line5  = LineShape(Vector2D(startPoint.get.x, startPoint.get.y+170), Vector2D(startPoint.get.x, startPoint.get.y+200), Attributes("Color" -> "#999999".color))
      val line1  = LineShape(Vector2D(startPoint.get.x, startPoint.get.y+200), Vector2D(startPoint.get.x, startPoint.get.y+220), Attributes("Color" -> "#999999".color))

      radians(gizmoMode).foreach(radian => {
        if (gizmoMode == 45)
          g draw line45.transform(t.rotate(radian, startPoint.get))
        else if (gizmoMode == 10)
          g draw line10.transform(t.rotate(radian, startPoint.get))
        else if (gizmoMode == 5)
          g draw line5.transform(t.rotate(radian, startPoint.get))
        else g draw line1.transform(t.rotate(radian, startPoint.get))
      })
    }
  }
}