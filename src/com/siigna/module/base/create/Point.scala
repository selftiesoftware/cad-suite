package com.siigna.module.base.create

/* 2012 (C) Copyright by Siigna, all rights reserved. */

/*
 SIIGNA POINT MODULE
 a module handling all situations where a point is needed.
 TODO: correct the angle guide rotation
 TODO: ability to using angle gizmo successively
 TODO: ability to write distances following angle snap
 TODO: fix a bug where the point module exits unexpectedly when AngleGizmoLoop finishes AFTER the Angle Gizmo.
 */

import com.siigna._

object Point extends Module {

  private var angleSnap : Option[AngleSnap] = None

  private var basePointSet = false
  //text input for X values
  private var coordinateX : Option[Double] = None

  //text input for Y values
  private var coordinateY : Option[Double] = None

  //input string for distances
  private var coordinateValue : String = ""

  private def difference : Vector2D = if (previousPoint.isDefined) previousPoint.get else Vector2D(0, 0)

  private var filteredX : Option[Double] = None

  /**
   * info about the main var:
   * var point
   * This is point the module is trying to establish.
   * It is found in several different in two ways.
   * Either directly by user input (MouseDown or Keys)
   * or by means of an Angle Gizmo that helps determining a point
   * as a radial offset from an existing point.
   * in all events, if a point is found,
   * the point var is returned to the calling module
   * using a Send(Message()) event.
   */
  private var point : Option[Vector2D] = None

  var pointGuide : Option[PointGuide] = None

  //Store the mousePosition, so we get the snap-coordinates
  private var mousePosition : Option[Vector2D] = None

  /**
   * The previous point saved as the relative value to the next.
   */
  private var previousPoint : Option[Vector2D] = None

  var snapAngle : Option[Double] = None

  // Save the X value, if any
  def x : Option[Double] = if (!coordinateX.isEmpty)
      coordinateX
    else if (coordinateValue.length > 0 && coordinateValue != "-")
      Some(java.lang.Double.parseDouble(coordinateValue) + difference.x)
    else if (coordinateX.isDefined)
      Some(coordinateX.get + difference.x)
    else None

  // Save the Y value, if any
  def y : Option[Double] = if (coordinateY.isDefined)
      coordinateY
    else if (coordinateX.isDefined && coordinateValue.length > 0 && coordinateValue != "-")
      Some(java.lang.Double.parseDouble(coordinateValue) + difference.y)
    else if (coordinateY.isDefined)
      Some(coordinateY.get + difference.y)
    else None

  val eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph[Symbol, Symbol](
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {

        // Check for continued MouseDown
        case Message(g : PointGuide) :: Message(p : Vector2D) :: MouseDown(_, MouseButtonLeft, _) :: tail => {
          pointGuide = Some(g)

          ForwardTo('AngleGizmo)
        }

        // Check for PointGuide
        case Message(g : PointGuide) :: tail => pointGuide = Some(g)

        case Message(a : AngleSnap) :: tail => eventParser.snapTo(a)

        // Exit strategy
        case (MouseDown(_, _, _) | MouseUp(_, _, _)) :: tail => Goto('End)

        // Mouse position
        case MouseMove(p, _, _) :: tail => mousePosition = Some(p)
        case MouseDrag(p, _, _) :: tail => mousePosition = Some(p)

        // Exit strategy
        case KeyDown(Key.Esc, _) :: tail => Goto('End)
        case KeyDown(Key.Backspace, _) :: tail => {
          if (coordinateValue.length > 0) coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
          else if (coordinateX.isDefined) {
            coordinateValue = coordinateX.get.toString
            coordinateX     = None
          }
        }
        //goto second coordinate if ENTER, COMMA, or TAB is pressed
        case KeyDown(Key.Enter | Key.Tab | ',', _) :: tail => {
          if (coordinateX.isEmpty && coordinateValue.length == 0) Goto('End)
          //when ENTER is pressed, and a value is det, this valus is passed as the first coordinate relative to 0,0
          if (coordinateX.isEmpty && coordinateValue.length > 0) {
            coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))
            //a hack used in paint to get the point input used to draw the position without transformation
            filteredX = Some(coordinateX.get + difference.x)

            coordinateValue = ""
          } else if (coordinateY.isEmpty && coordinateValue.length > 0) {
            coordinateY = Some(java.lang.Double.parseDouble(coordinateValue))
            coordinateValue = ""
            //Goto('End)
          }
        }
        case KeyDown(Key.Space, _) :: tail => Goto('End)

        //get the input from the keyboard if it is numbers, (-) or (.)
        case KeyDown(code, _) :: tail => {
          val char = code.toChar
          if (char.isDigit) {
            coordinateValue += char
          }
          else if ((char == '.') && !coordinateValue.contains('.'))
            coordinateValue += "."
          else if (char == '-' && coordinateValue.length < 1)
            coordinateValue = "-"
        }
        case KeyUp(Key.Space, _) :: tail => Goto ('End)
        case _ =>
      }
      // END CASE MATCHES.

      // Format the x and y coordinate and store the message in a value
      val message = if (coordinateValue.length > 0) {
        val x = if (coordinateX.isDefined) "%.3f" format coordinateX.get
                else coordinateValue
        val y = if (coordinateY.isDefined) "%.3f" format coordinateY.get
                else if (coordinateX.isDefined) coordinateValue
                else ""

        // Save the message
        "point (X: "+x+", Y: "+y+")."
      } else if (mousePosition.isDefined) {
        val x = "%.3f" format (if (coordinateX.isDefined) coordinateX.get else mousePosition.get.x)
        val y = "%.3f" format mousePosition.get.y

        "point (X: "+x+", Y: "+y+")."
      } else {
        "click or type point"
      }

      // Display the message
      Siigna display message

      //if the next point has been typed, add it to the polyline:

      if (coordinateX.isDefined && coordinateY.isDefined ) {

        //convert the relative coordinates a global point by adding the latest point
        val x = coordinateX.get + difference.x
        val y = coordinateY.get + difference.y

        //add the typed point to the polyline
        point = Some(Vector2D(x,y))
        //clear the coordinate vars
        coordinateX = None
        coordinateY = None
        coordinateValue = ""
        Goto('End)
      }
    }),
    'End -> ((events : List[Event]) => {
      //Clear the variables
      coordinateX = None
      coordinateY = None
      coordinateValue = ""
      filteredX = None
      eventParser.clearSnap()

      // Reset the point guide
      pointGuide = None
      previousPoint = point

      //clear the point.
      val p = point
      point = None

      // Return a point if it is defined
      if (p.isDefined) {

        // Return the point
        Message(p)
      } else events match {
        case MouseDown(p, _, _) :: tail => Message(p)
        case MouseUp(p, MouseButtonLeft, _) :: tail => Message(p)
        case _ =>
      }
    }
  ))

  override def paint(g : Graphics, t : TransformationMatrix) {
    // Draw a point guide with the new point as a parameter

    //input:
    //   Vector2D. A guide (the current point / mouse position

    //return:
    // Immutable Shape : if the calling module has passed a shape it is drawn, including the dynamic part.

    // Draw a point guide if a previous point (or guide) is found.
    if (pointGuide.isDefined || previousPoint.isDefined) {
      val guide : Vector2D => ImmutableShape = {
        //if there is no previous point, use
        if (pointGuide.isDefined) {
          pointGuide.get.guide
        } else {
          //the last field _ is replaceable depending on how the point is constructed
          LineShape(previousPoint.get, _)
        }
      }

      // Draw the point guide depending on which information is available
      if (x.isDefined && y.isDefined) {
        g draw guide(Vector2D(x.get + difference.x, y.get)).transform(t)
      } else if (x.isDefined && mousePosition.isDefined && !filteredX.isDefined) {
        g draw guide(Vector2D(x.get, mousePosition.get.y)).transform(t)
      } else if (x.isDefined && mousePosition.isDefined && filteredX.isDefined) {
        g draw guide(Vector2D(filteredX.get, mousePosition.get.y)).transform(t)
      } else if (mousePosition.isDefined && !angleSnap.isDefined) {
        g draw guide(mousePosition.get).transform(t)
      } else if (mousePosition.isDefined && angleSnap.isDefined) {
        g draw LineShape(angleSnap.get.center, mousePosition.get).transform(t)
      }
    }

    //If angle snap is activated, draw the snap guide
    if(angleSnap.isDefined && mousePosition.isDefined) {
      g draw LineShape(angleSnap.get.center, angleSnap.get.snapToRadian(mousePosition.get)).transform(t)
    }
  }

}

/**
 * A class used to draw guides in the point module.
 */
case class PointGuide(guide : Vector2D => ImmutableShape)