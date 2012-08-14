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
import com.siigna.module.base.Default._

/**
 * The point module answers to requests from many other modules who require a number or one or more points to function.
 *
 * No messages need to be send in order to call the point module. To get a point from the point module, use ForwardTo('Point).
 * The point module will run and return a point (if one has been set) as a Message(Vector2D).
 *
 * Points are set with the mouse or by typing an x and y value, seperated by a comma.
 *
 * Except when the 'Rotate, 'Scale, or 'Move (if track is active) modules call 'Point,
 * in which cases only one value is entered, and a Message(Double) is returned.
 *
 * It is possible to send a Controller ! Message(Guide(ShapeGuide)) to the 'Point module when it is called
 * if the Point need to draw shapes dynamically while input is entered.
 */

object Point extends Module {

  private var angleOrScale : Option[Double] = None

  //a placeholder for the active AngleSnap - needed if the user wants to type a length of a line segment
  private var currentSnap : Option[AngleSnap] = None

  private var basePointSet = false
  //text input for X values
  private var coordinateX : Option[Double] = None

  //text input for Y values
  private var coordinateY : Option[Double] = None

  //input string for distances
  private var coordinateValue : String = ""

  private var decimalValue : Boolean = false
  
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
      //display the name of the active module:
      //Siigna display com.siigna.module.base.Default.previousModule.get.toString

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
        //use period to define decimal numbers
        case KeyDown('.', _) :: tail => {
          if(coordinateValue.isEmpty) decimalValue = true
          else coordinateValue += '.'
        }

        //goto second coordinate if ENTER, COMMA, or TAB is pressed
        case KeyDown(Key.Enter | Key.Tab | (','), _) :: tail => {

          //move
          if (angleOrScale.isDefined && moving == true && !coordinateValue.isEmpty) {
            distance = Some(coordinateValue.toDouble)
            Goto('End)
          }
          //if a distance on a track radial has been entered, return the parsed point:
          //NOTE: it sis necassary to check if rotate of scale is calling, because in this case keyDown should not set a point, but just a value (by running setting the angle var)
          if (eventParser.isTracking == true && !coordinateValue.isEmpty && previousModule != Some('Rotate) && previousModule != Some('Scale)) {
            point = Track.getPointFromDistance(coordinateValue.toDouble)
            Goto('End)
          }
          //if noting is entered
          else if (!currentSnap.isDefined && coordinateX.isEmpty && coordinateValue.length == 0) Goto('End)
          //when ENTER is pressed, and a value is set, this value is passed as the first coordinate relative to 0,0
          else if (!currentSnap.isDefined && coordinateX.isEmpty && coordinateValue.length > 0) {
            coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))

            //rotate
            if (previousModule == Some('Rotate) || previousModule == Some('Scale)) {
              if(coordinateX.isDefined) {
                angleOrScale = Some(coordinateX.get)
                Goto('End)
              }
            }

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
          if (char.isDigit)
            coordinateValue += char
          else if ((char == '.') && !coordinateValue.contains('.'))
            coordinateValue += "."
          else if (char == '-' && coordinateValue.length < 1)
            coordinateValue = "-"
        }
        case KeyUp(Key.Space, _) :: tail => Goto ('End)
        case _ =>
      }
      // END CASE MATCHES.
      //reformat the coordinate value to a decimal number if the input string started with '.' (decimalValue flag is true)
      if (decimalValue == true && !coordinateValue.isEmpty && coordinateValue != 0) coordinateValue = (coordinateValue.toDouble/(coordinateValue.length +1)).toString

      // DISPLAY a message: Format the x and y coordinate and store the message in a value, unless Rotate or Scale is calling, since they need just one value.
      val message =
        if (coordinateValue.length > 0  && previousModule != Some('Rotate) && previousModule != Some('Scale)) {

        val x = if (coordinateX.isDefined) "%.2f" format coordinateX.get
                else coordinateValue
        val y = if (coordinateY.isDefined) "%.2f" format coordinateY.get
                else if (coordinateX.isDefined) coordinateValue
                else ""

        Some("point (X: "+x+", Y: "+y+").")

        //Display the coords when typed.
      } else if (mouseLocation.isDefined && previousModule != Some('Rotate) && moving == false && coordinateX.isDefined) {
        var prevModule = com.siigna.module.base.Default.previousModule.get.toString
        Some("point (X: "+x+")")
        }

        //typing a move point
        else if (mouseLocation.isDefined && previousModule != Some('Rotate) && moving == true && !angleOrScale.isDefined) {
        val x = "%.2f" format (if (coordinateX.isDefined) coordinateX.get else mouseLocation.get.x)
        val y = "%.2f" format mouseLocation.get.y
        Some("point (X: "+x+", Y: "+y+").")
        }
        else if (previousModule == Some('Rotate) && !x.isDefined) {
          Some("set origin or type angle: "+ coordinateValue)
        }
        else if (previousModule == Some('Scale) && !x.isDefined) {
          Some("type or click to set scale factor: "+ coordinateValue)
        }
        else if (previousModule == Some('Rotate) && coordinateValue.length > 0) {
          Some("rotation angle: "+ coordinateValue)
        }
        else if (previousModule == Some('Scale) && coordinateValue.length > 0) {
          Some("scale factor: "+ coordinateValue)
        }
        else if (moving == true && angleOrScale.isDefined) {
          Some("distance: "+ coordinateValue)
        }
        else {
         None
      }

      // Display the message
      if(message.isDefined) Siigna display(message.get)

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
      val r = angleOrScale

      //clear vars.
      angleOrScale = None
      coordinateX = None
      coordinateY = None
      coordinateValue = ""
      decimalValue = false
      currentSnap = None
      distance = None
      eventParser.clearSnap()
      filteredX = None
      moving = false
      point = None

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
      // Return an angle / scale if it is defined
      else if (r.isDefined){
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

      // DRAW THE POINT GUIDE BASED ON THE INFORMATION AVAILABLE:

      //If tracking is active
      if(!coordinateValue.isEmpty && mouseLocation.isDefined && eventParser.isTracking == true) {
        var trackPoint = Track.getPointFromDistance(coordinateValue.toDouble)
        guide(trackPoint.get).foreach(s => g draw s.transform(t))
      }
      //If a set of coordinates have been typed
      if (x.isDefined && y.isDefined) guide(Vector2D(x.get + difference.x, y.get)).foreach(s => g draw s.transform(t))
      else if (x.isDefined && mouseLocation.isDefined && !filteredX.isDefined && !currentSnap.isDefined && eventParser.isTracking == false) guide(Vector2D(x.get, mouseLocation.get.y)).foreach(s => g draw s.transform(t))
      else if (x.isDefined && mouseLocation.isDefined && !filteredX.isDefined && currentSnap.isDefined && eventParser.isTracking == false) guide(lengthVector(x.get - difference.x)).foreach(s => g draw s.transform(t))
      else if (x.isDefined && mouseLocation.isDefined && filteredX.isDefined  && eventParser.isTracking == false) guide(Vector2D(filteredX.get, mouseLocation.get.y)).foreach(s => g draw s.transform(t))
      else if (mouseLocation.isDefined  && coordinateValue.isEmpty) guide(mouseLocation.get).foreach(s => g draw s.transform(t))

      else None
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