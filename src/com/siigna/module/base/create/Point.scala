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

object Point extends Module {

  private var angle : Option[Double] = None

  //a placeholder for the active AngleSnap - needed if the user wants to type a length of a line segment
  private var currentSnap : Option[AngleSnap] = None

  private var basePointSet = false
  //text input for X values
  private var coordinateX : Option[Double] = None

  //text input for Y values
  private var coordinateY : Option[Double] = None

  //input string for distances
  private var coordinateValue : String = ""

  private def difference : Vector2D = if (previousPoint.isDefined) previousPoint.get else Vector2D(0, 0)

  private var distance : Option[Double] = None

  private var filteredX : Option[Double] = None

  //a function to add a typed distance to a line, after the Angle Gizmo has redined a radial.
  private def lengthVector(length : Double) : Vector2D = {
    //a vector that equals the length of the typed distance, rotated by the current radial snap setting.
    var rotatedVector = Vector2D(math.sin(currentSnap.get.degree * math.Pi/180), math.cos(currentSnap.get.degree * math.Pi/180)) * length
    //and transformed by the center point of the offset from the Angle snap gizmo.
    rotatedVector + currentSnap.get.center
  }

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

  var pointGuide : Option[Guide] = None

  var moving : Boolean = false

  //Store the mousePosition, so we get the snap-coordinates
  private var mouseLocation : Option[Vector2D] = None

  /**
   * The previous point saved as the relative value to the next.
   */
  private var previousPoint : Option[Vector2D] = None

  var rotation : Boolean = false

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
      println(Siigna.navigation)
      if (com.siigna.module.base.Default.previousModule == Some('Rotate)) rotation = true
      if (com.siigna.module.base.Default.previousModule == Some('Move)) moving = true
      events match {

        // Check for continued MouseDown
        case Message(g : Guide) :: Message(p : Vector2D) :: MouseDown(_, MouseButtonLeft, _) :: tail => {
          pointGuide = Some(g)
          ForwardTo('AngleGizmo)
        }

        // Check for PointGuide
        case Message(g : Guide) :: tail => {
          pointGuide = Some(g)
        }

        case Message(a : AngleSnap) :: tail => {
          currentSnap = Some(a)
          eventParser.snapTo(a)
        }
        // Avoid ending if the mouse up comes after setting the angle in the AngleGizmo
        case MouseDown(p, MouseButtonLeft, _) :: Message(a : AngleSnap) :: tail => previousPoint = Some(p)

        case MouseUp(_, MouseButtonLeft, _) :: Message(a : AngleSnap) :: tail =>

        // Exit strategy
        case (MouseDown(_, _, _) | MouseUp(_ , MouseButtonRight, _)) :: tail => {
          Goto('End)
        }

        // Mouse position
        case MouseMove(p, _, _) :: tail => mouseLocation = Some(p)
        //case MouseDrag(p, _, _) :: tail => mousePosition = Some(p)

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
          if (rotation == true && coordinateX.isDefined) {
            angle = Some(coordinateX.get)
            Goto('End)
          }
          if (angle.isDefined && moving == true && !coordinateValue.isEmpty) {
            distance = Some(coordinateValue.toDouble)
            Goto('End)
          }
          else if (!currentSnap.isDefined && coordinateX.isEmpty && coordinateValue.length == 0) Goto('End)
          //when ENTER is pressed, and a value is det, this valus is passed as the first coordinate relative to 0,0
          else if (!currentSnap.isDefined && coordinateX.isEmpty && coordinateValue.length > 0) {
            coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))
            //a hack used in paint to get the point input used to draw the position without transformation
            filteredX = Some(coordinateX.get + difference.x)

            coordinateValue = ""
          }
          //if the angle gizmo is used, and a length has been typed, send the resulting point
          else if(currentSnap.isDefined && x.isDefined) {
              point = Some(lengthVector(x.get - difference.x))
            Goto('End)
          }
          else if (coordinateY.isEmpty && coordinateValue.length > 0) {
            coordinateY = Some(java.lang.Double.parseDouble(coordinateValue))
            coordinateValue = ""
            //Goto('End)
          }
        }
        case KeyDown(Key.Shift, _) :: tail => {
          Controller ! Message(previousPoint.get)
          ForwardTo('AngleGizmo)
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
      val message = if (coordinateValue.length > 0 ) {
        val x = if (coordinateX.isDefined) "%.3f" format coordinateX.get
                else coordinateValue
        val y = if (coordinateY.isDefined) "%.3f" format coordinateY.get
                else if (coordinateX.isDefined) coordinateValue
                else ""

        // Save the message
        "point (X: "+x+", Y: "+y+")."
      } else if (mouseLocation.isDefined && rotation == false && moving == false) {
        val x = "%.3f" format (if (coordinateX.isDefined) coordinateX.get else mouseLocation.get.x)
        val y = "%.3f" format mouseLocation.get.y
        "point (X: "+x+", Y: "+y+")."
        }

        //typing a move point
        else if (mouseLocation.isDefined && rotation == false && moving == true && !angle.isDefined) {
        val x = "%.3f" format (if (coordinateX.isDefined) coordinateX.get else mouseLocation.get.x)
        val y = "%.3f" format mouseLocation.get.y
        "point (X: "+x+", Y: "+y+")."
        }

        else if (rotation == true && x.isDefined) {
          "rotation angle: "+ x.get
        }
        else if (moving == true && angle.isDefined) {
          "distance: "+ coordinateValue
        }
        else {
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
      // If a point was set (unless it is the first point, in which case it will be set in 'End), save it as the PreviousPoint.
      if(point.isDefined) {
        previousPoint = point
      }
      //transfer variables to be able to use them for sending messages after module variables have been cleared
      val d = distance
      val distAngle = currentSnap
      val p = point
      val r = angle

      //clear vars.
      angle = None
      coordinateX = None
      coordinateY = None
      coordinateValue = ""
      currentSnap = None
      distance = None
      eventParser.clearSnap()
      filteredX = None
      moving = false
      point = None
      rotation = false

      // Return a point if it is defined
      if (p.isDefined) {
        // Return the point
        Message(p.get)
      }
      // Return a point for moving, if one has been typed and the Angle Gizmo used to specify an angle for the move operation.
      else if (d.isDefined && distAngle.isDefined) {
        // Calculate the point based on angle and distance, and return it.
        val t : TransformationMatrix = TransformationMatrix(Vector2D(0,0), 1).rotate(distAngle.get.degree)
        val m = Vector2D(d.get,0).transform(t)
        Message(m)

      }
      // Return an angle if it is defined
      else if (r.isDefined) {
        Message(r.get)
      }
      // if it is the first point, return it:
      else events match {
        case MouseDown(p, MouseButtonLeft, _) :: tail => {
          previousPoint = Some(p)
          Message(p)

        }
        case MouseUp(p, MouseButtonLeft, _) :: tail => {
          previousPoint = Some(p)
          Message(p)
        }
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
      val guide : Vector2D => Traversable[Shape] = {
        //if there is no previous point, use
        if (pointGuide.isDefined) {
          pointGuide.get
        } else {
          //the last field _ is replaceable depending on how the point is constructed
          p => Traversable(LineShape(previousPoint.get, p))
        }
      }

      // Draw the point guide depending on which information is available
      if (x.isDefined && y.isDefined) {
        guide(Vector2D(x.get + difference.x, y.get)).foreach(s => g draw s.transform(t))
      } else if (x.isDefined && mouseLocation.isDefined && !filteredX.isDefined && !currentSnap.isDefined) {
        guide(Vector2D(x.get, mouseLocation.get.y)).foreach(s => g draw s.transform(t))
      } else if (x.isDefined && mouseLocation.isDefined && !filteredX.isDefined && currentSnap.isDefined) {
        guide(lengthVector(x.get - difference.x)).foreach(s => g draw s.transform(t))
      } else if (x.isDefined && mouseLocation.isDefined && filteredX.isDefined) {
        guide(Vector2D(filteredX.get, mouseLocation.get.y)).foreach(s => g draw s.transform(t))
      } else if (mouseLocation.isDefined) {
        guide(mouseLocation.get).foreach(s => g draw s.transform(t))
      } else None
    }
  }

}

trait Guide extends (Vector2D => Traversable[Shape])

/**
 * A class used to draw guides in the point module.
 */
case class PointGuide(guide : Vector2D => Shape) extends Guide {
  def apply(v : Vector2D) = Traversable(guide(v))
}
case class PointGuides(guide : Vector2D => Traversable[Shape]) extends Guide{
  def apply(v : Vector2D) = guide(v)
}

object PointGuide {
  def apply(part : PartialShape) = new PointGuide(p => part(TransformationMatrix(p, 1)))
}