package com.siigna.module.endogenous

import com.siigna._

/**
 * An object that handles the angle-gizmo.
 */
//TODO: multiply with zoomscale to get a fixed size gizmo

object AngleGizmo extends Module {

  var activeAngle : Double = 0
  var finalAngle : Option[Double] = None

  //time to press and hold the mouse button before the gizmo mode is activated
  val gizmoTime = 500

  var latestEvent : Option[Event] = None

  var gizmoMode = 45
  val gizmoRadius = 220
  val gizmoShapes = List[Shape]()
  //var runGizmo = true

  var guideLength = 0

  var startPoint : Option[Vector2D] = None
  var mousePosition : Option[Vector2D] = None

  var points   = List[Vector2D]()

  var receivedPoint : Option[Vector2D] = None

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
      activeRadians = i :: activeRadians
      i += mode

    } while (i <= 360)
    activeRadians
  }

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start         -> 'KeyEscape -> 'End
    //'AngleGizmo    -> 'MouseDown   -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      // Start the loop
      val a = new AngleGizmoLoop
      a.start()

      // Define the latest event
      latestEvent = Some(events.head)

      // Listen to mouse-events
      Goto('MouseCheck)
    }),
    //check if a mouse up is occuring while running the angle gizmo loop, if so, the angle module will exit.
    'MouseCheck -> ((events : List[Event]) => {
      events match {
        //if these movements are registered while the AngleGizmoLoop is running, stay in this Node
        case MouseDown(_, _, _) :: tail =>
        case MouseDrag(point, _, _) :: tail => {
          latestEvent = Some(events.head)
        }
        case MouseUp(p, _, _) :: tail =>
        //if not, goto 'End
        case _ => Goto('End)
      }
    }),
    'AngleGizmo -> ((events : List[Event]) => {
      //reaching this state means the gizmo should be drawn, so
      //the start point is set as the received point from the calling module
      startPoint = receivedPoint
      events match {
        //if the right mouse button is pressed, exit.
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        //the latest event coming from polyline has to be mouse down, so this event forms the basis for getting the current mouse position:
        case MouseDown(point, MouseButtonLeft, _) :: MouseMove(_, _, _) :: tail => {
          points = points :+ point

          //if a new point is set, goto 'End
          if (points.size > 1) {
            mousePosition = Some(point)
            Goto('End)
          }
        }
        case MouseMove(point, _, _) :: tail => {
          mousePosition = Some(point)
        }
        case MouseDrag(point, _, _) :: tail => {
          mousePosition = Some(point)
        }
        case _=>
      }
      //get the current radial
      if (mousePosition.isDefined && startPoint.isDefined) {
        var radian = (mousePosition.get - startPoint.get).angle.toInt
        var calculatedAngle = radian * -1 + 450
        if (calculatedAngle > 360)
          {activeAngle = calculatedAngle - 360} else activeAngle = calculatedAngle
      }
    }),

    //return the output of the anonymous function f, declared above the StateMachine
    'End -> ((events : List[Event]) => {
      points = List[Vector2D]()
      receivedPoint = None
      startPoint = None
      Message(activeAngle)
    })
  )

  //Draw the Angle Gizmo perimeter
  override def paint(g : Graphics, t : TransformationMatrix) {
      //println("events in angle gizmo paint: "+latestEvent)
    if (startPoint.isDefined && mousePosition.isDefined) {
      //Set Angle Gizmo mode based on distance to center
      def distanceToStart = mousePosition.get - startPoint.get
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
      g draw TextShape((correct360(round(activeAngle).toInt)).toString, Vector2D(startPoint.get.x, startPoint.get.y + 240).transform(t.rotate(round(-activeAngle), startPoint.get)), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector2D(0.5,0.5)))
      g draw guide.transform(t.rotate((((round(activeAngle)* -1)+360).toInt), startPoint.get))

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