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

  private var decimalValue : Boolean = false


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



  val stateMap: StateMap = Map(

    'Start -> {
      case MouseDown(p,button,modifier)::tail => {
        // End and return mouse-position-point, if left mouse button is clicked
        if (button==MouseButtonLeft) {
          End(p.transform(View.deviceTransformation))
        } else {
          // In all other cases, the mouseDown is returned
          End(MouseDown(p,button,modifier))
        }
      }

      // Check for PointGuide
      case Start(_ ,g : Guide) :: tail => {
        pointGuide = Some(g)
      }

      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      // End and return backspace, if backspace is pressed
      case KeyDown(Key.Backspace, _) :: tail => {
        println("Backspace pressed")
        End(KeyDown(Key.Backspace,ModifierKeys(false,false,false)))
      }

      // All other keyDown events are forwarded:
      case KeyDown(key,modifier) :: tail => End(KeyDown(key,modifier))

      case _ =>  {

        //reformat the coordinate value to a decimal number if the input string started with '.' (decimalValue flag is true)

        //if (decimalValue == true && !coordinateValue.isEmpty && coordinateValue != 0)
        //  coordinateValue = (coordinateValue.toDouble/(coordinateValue.length +1)).toString

        // DISPLAY a message: Format the x and y coordinate and store the message in a value.
        /*val message = {
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
        */
        // Display the message
        //if(message.isDefined) Siigna display(message.get)
      }
    }//,
    /*'End -> {
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
    } */
  )


  override def paint(g : Graphics, t : TransformationMatrix) {

    pointGuide.foreach(_ .guide(mousePosition.transform(View.deviceTransformation)).foreach(s => g.draw(s.transform(t))))

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