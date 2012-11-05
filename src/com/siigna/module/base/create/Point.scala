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
 * The point module answers to requests from many other modules who require a number or one or more points to function.
 *
 * No messages need to be send in order to call the point module. To get a point from the point module, use Module('Point).
 * The point module will run and return a point (if one has been set) as a Message(Vector2D).
 *
 * Points are set with the mouse or by typing an x and y value, seperated by a comma.
 *
 *
 * It is possible to send a PointGuide to the 'Point module when it is called
 * if the Point module needs to draw shapes dynamically while input is entered.
 */

class Point extends Module {

  //text input for X values
  private var coordinateX : Option[Double] = None

  //text input for Y values
  private var coordinateY : Option[Double] = None

  //input string for distances
  private var coordinateValue : String = ""

  private var decimalValue : Boolean = false

  private def difference : Vector2D = if (previousPoint.isDefined) previousPoint.get else Vector2D(0, 0)

  //private var distance : Option[Double] = None

  //private var filteredX : Option[Double] = None

  //a function to add a typed distance to a line, after the Angle Gizmo has redined a radial.
  //private def lengthVector(length : Double) : Vector2D = {
  //a vector that equals the length of the typed distance, rotated by the current radial snap setting.
  //var rotatedVector = Vector2D(math.sin(currentSnap.get.degree * math.Pi/180), math.cos(currentSnap.get.degree * math.Pi/180)) * length
  //and transformed by the center point of the offset from the Angle snap gizmo.
  //rotatedVector + currentSnap.get.center


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

  val stateMap: StateMap = Map(

    'Start -> {
      case MouseDown(p,button,modifier)::tail => {
        if (button==MouseButtonLeft) {
          println("B")
          End(p.transform(View.deviceTransformation))
        } else {
          // Exit on right mouse button
          End
        }
      }

      // Check for PointGuide
      case Start(_ ,g : Guide) :: tail => {
        pointGuide = Some(g)
      }
     // case Start(_ ,g : Guide) :: tail => {
     //   pointGuide = Some(g)
     // }

      // Check for continued MouseDown
      //case Message(g : Guide) :: Message(p : Vector2D) :: MouseDown(_, MouseButtonLeft, _) :: tail => {
      //  pointGuide = Some(g)
      //Module('AngleGizmo)
      //}

      //case Message(a : AngleSnap) :: tail => {
      //currentSnap = Some(a)
      //  eventParser.snapTo(a)
      //}

      // Avoid ending if the mouse up comes after setting the angle in the AngleGizmo
      //case MouseDown(p, MouseButtonLeft, _) :: Message(a : AngleSnap) :: tail => previousPoint = Some(p)

      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      case KeyDown(Key.Backspace, _) :: tail => {
        if (coordinateValue.length > 0) coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
        else if (coordinateX.isDefined) {
          coordinateValue = coordinateX.get.toString
          coordinateX     = None
        }
      }
      case (MouseDown(_, _, _) | MouseUp(_ , MouseButtonRight, _)) :: tail => End
      //use period to define decimal numbers
      case KeyDown('.', _) :: tail => {
        if(coordinateValue.isEmpty) decimalValue = true
        else coordinateValue += '.'
      }

      //goto second coordinate if ENTER, COMMA, or TAB is pressed
      case KeyDown(Key.Enter | Key.Tab | (','), _) :: tail => {

        //if noting is entered
        if (coordinateX.isEmpty && coordinateValue.length == 0) End
        //when ENTER is pressed, and a value is set, this value is passed as the first coordinate relative to 0,0
        else if (coordinateX.isEmpty && coordinateValue.length > 0) {
          coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))

          coordinateValue = ""
        }
        else if (coordinateY.isEmpty && coordinateValue.length > 0) {
          coordinateY = Some(java.lang.Double.parseDouble(coordinateValue))
          coordinateValue = ""
          'End
        }
      }

      case KeyDown(Key.Shift, _) :: tail => {
        Controller ! Message(previousPoint.get)
        println("open Angle Gizmo here")
        //Module('AngleGizmo)
      }
      case KeyDown(Key.Space, _) :: tail => End

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
      //case KeyUp(Key.Space, _) :: tail => Goto ('End)
      case _ =>  {

        //reformat the coordinate value to a decimal number if the input string started with '.' (decimalValue flag is true)

        //if (decimalValue == true && !coordinateValue.isEmpty && coordinateValue != 0)
        //  coordinateValue = (coordinateValue.toDouble/(coordinateValue.length +1)).toString

        // DISPLAY a message: Format the x and y coordinate and store the message in a value.
        val message = {
          if (coordinateValue.length > 0 ) {

            val x = if (coordinateX.isDefined) "%.2f" format coordinateX.get
            else coordinateValue
            val y = if (coordinateY.isDefined) "%.2f" format coordinateY.get
            else if (coordinateX.isDefined) coordinateValue
            else ""

            Some("point (X: "+x+", Y: "+y+").")

            //Display the coords when typed.
          } else if (coordinateX.isDefined) {
            //var prevModule = com.siigna.module.base.ModuleInit.lastModule.get.toString
            Some("point (X: "+x+")")
          }
          else {
            None
          }
        }
        // Display the message
        if(message.isDefined) Siigna display(message.get)
      }
    },
    'End -> {
      case _ => {

        //if the next point has been typed, return it:
        if (coordinateX.isDefined && coordinateY.isDefined ) {
          println("IN END WITH COORDS")
          //convert the relative coordinates a global point by adding the latest point
          val x = coordinateX.get
          val y = coordinateY.get

          //add the typed point to the polyline
          point = Some(Vector2D(x,y))
          //clear the coordinate vars
          coordinateX = None
          coordinateY = None
          coordinateValue = ""

          println("IN POINT - END")
          // If a point was set (unless it is the first point, in which case it will be set in 'End), save it as the PreviousPoint.
          //if(point.isDefined) {
          //  previousPoint = point
          //}

          // Return a point if it is defined
          if (point.isDefined) {
            // Return the point
            End(point.get)
          }
        }
      }
    }
  )


  override def paint(g : Graphics, t : TransformationMatrix) {

    pointGuide.foreach(_ .guide(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t))))

    // Draw a point guide with the new point as a parameter

    //input:
    //   Vector2D. A guide (the current point / mouse position

    //return:
    // Immutable Shape : if the calling module has passed a shape it is drawn, including the dynamic part.

    // Draw a point guide if a previous point (or guide) is found.
    //if (pointGuide.isDefined || previousPoint.isDefined) {

    //  } else {
    //the last field _ is replaceable depending on how the point is constructed
    //  p => Traversable(LineShape(previousPoint.get, p))

    //  }

    // DRAW THE POINT GUIDE BASED ON THE INFORMATION AVAILABLE:

    //If tracking is active
    //  if(!coordinateValue.isEmpty && mouseLocation.isDefined && eventParser.isTracking == true) {
    //    var trackPoint = Track.getPointFromDistance(coordinateValue.toDouble)
    //    guide(trackPoint.get).foreach(s => g draw s.transform(t))
    //  }
    //If a set of coordinates have been typed
    //  if (x.isDefined && y.isDefined) guide(Vector2D(x.get + difference.x, y.get)).foreach(s => g draw s.transform(t))
    //  else if (x.isDefined && mouseLocation.isDefined && !filteredX.isDefined && !currentSnap.isDefined && eventParser.isTracking == false) guide(Vector2D(x.get, mouseLocation.get.y)).foreach(s => g draw s.transform(t))
    //  else if (x.isDefined && mouseLocation.isDefined && !filteredX.isDefined && currentSnap.isDefined && eventParser.isTracking == false) guide(lengthVector(x.get - difference.x)).foreach(s => g draw s.transform(t))
    //  else if (x.isDefined && mouseLocation.isDefined && filteredX.isDefined  && eventParser.isTracking == false) guide(Vector2D(filteredX.get, mouseLocation.get.y)).foreach(s => g draw s.transform(t))
    //  else if (mouseLocation.isDefined  && coordinateValue.isEmpty) guide(mouseLocation.get).foreach(s => g draw s.transform(t))
    //if (coordinateValue.isEmpty) guide(mousePosition).foreach(s => g draw s.transform(t))

    //  else None
  }
}

/**
 * A class used to draw guides in the point module.
 */

case class Guide(guide : Vector2D => Traversable[Shape])

//a case class where the point can be extracted.
case class PointGuide(point : Vector2D , guide : Vector2D => Traversable[Shape])

/*
object PointGuide {
  def apply(part : PartialShape) = new PointGuide(p => part(TransformationMatrix(p, 1)))
}*/