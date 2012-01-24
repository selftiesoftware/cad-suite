/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Point extends Module {

  // The AngleGuide is the guide that comes from the AngleGizmo
  private var angleGuide : Option[Double] = None

  // The anglePoint is the point where the angle gizmo is centered, and thus
  // the point where a possible future angleGuide has to extend from
  private var anglePoint : Option[Vector2D] = None

  //text input for X values
  private var coordinateX : Option[Double] = None

  //text input for Y values
  private var coordinateY : Option[Double] = None

  //input string for distances
  private var coordinateValue : String = ""

  def difference : Vector2D = if (previousPoint.isDefined) previousPoint.get else Vector2D(0, 0)

  private var filteredX : Option[Double] = None

  // Store the mousePosition, so we get the snap-coordinates
  private var mousePosition : Option[Vector2D] = None

  // The point
  private var point : Option[Vector2D] = None

  var pointGuide : Option[PointGuide] = None

  var previousPoint : Option[Vector2D] = None

  // The polylineshape so far
  private var shape : Option[Shape] = None

  //Preload AngleGizmo
  Preload('AngleGizmo, "com.siigna.module.endogenous.AngleGizmo")

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

  def stateMap = DirectedGraph(
    'Start         -> 'MouseUp   -> 'End,
    'Start         -> 'KeyEscape   -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      println("incoming events: "+events.head)
      events match {

        //FIRST ENTRY IS ALWAYS HERE, (unless returning from Angle Gizmo, in which case the module will start in 'Message
        // The latest event (MouseDown) from the calling module is executed now:
        case MouseDown(p, MouseButtonLeft, _):: tail => {
          println("PT START: "+p)
          //get the point
          point = Some(p)

          anglePoint = Some(Siigna.mousePosition)
         //GOTO ANGLE GIZMO
         ForwardTo('AngleGizmo)
        }

        case Message(g : PointGuide) :: tail => {
          //if the module receives a point guide, assign this to the var pointGuide
          pointGuide = Some(g)
        }

        case MouseMove(p, _, _) :: tail => mousePosition = Some(p)

        //check to see if the gizmo successfully returned an angle
        case Message(p : Double) :: tail => {
          println("returning 400 from angle gizmo")
          //if no message was returned and the point is set:
          if(p == 400 && point.isDefined)
            Goto('End)
          else {
            angleGuide = Some(p)

            //Since we got the angle we can now snap to the center point and the angle
            eventParser.snapTo(new AngleSnap(anglePoint.get, p))
            //Goto('End)
          }
        }

        case MouseDrag(point, _, _) :: tail => mousePosition = Some(point)
        case MouseDown(_, MouseButtonRight, _) :: tail => {
          //exit without sending on a point, without sending the latest event (mouseDown) as it would exit the calling module as well.
          point = None
          Goto('End)
        }
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
    }
  ),
    'End -> ((events : List[Event]) => {
      //Clear the variables
      shape = None
      //point = None
      angleGuide = None
      anglePoint = None
      coordinateX = None
      coordinateY = None
      coordinateValue = ""
      filteredX = None

      // Reset the point guide
      pointGuide = None
      previousPoint = point

      // Return a point if it was defined
      println("END: point: "+point)
      if(point.isDefined)
        Message(point.get)
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
      }
      else if (x.isDefined && mousePosition.isDefined && !filteredX.isDefined) {
        g draw guide(Vector2D(x.get, mousePosition.get.y)).transform(t)
      }
      else if (x.isDefined && mousePosition.isDefined && filteredX.isDefined) {
        g draw guide(Vector2D(filteredX.get, mousePosition.get.y)).transform(t)
      }
      else if (mousePosition.isDefined)
        g draw guide(mousePosition.get).transform(t)
    }
  }

}

/**
 * A class used to draw guides in the point module.
 */
case class PointGuide(guide : Vector2D => ImmutableShape)