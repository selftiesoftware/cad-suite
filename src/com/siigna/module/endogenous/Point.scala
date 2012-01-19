/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import java.awt.Color

import com.siigna._
import app.controller.ModuleBank

object Point extends Module {

  var isGizmoCheckNeeded = false

  var pointGuide : Option[PointGuide] = None

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

  // Store the mousePosition, so we get the snap-coordinates
  private var mousePosition : Option[Vector2D] = None

  // The point
  private var point : Option[Vector2D] = None

  private var unfilteredX : Option[Double] = None

  // The polylineshape so far
  private var shape : PolylineShape = PolylineShape.empty

  // Preload AngleGizmo
  //Preload('AngleGizmo, "com.siigna.module.endogenous.AngleGizmo")

  // Save the X value, if any
  def x : Option[Double] = if (!coordinateX.isEmpty)
      coordinateX
    else None

  // Save the Y value, if any
  def y : Option[Double] = if (coordinateY.isDefined)
      coordinateY
    else None

  val eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start         -> 'MouseUp   -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      println("in point")
      events match {
        case MouseMove(point, _, _) :: tail => mousePosition = Some(point)
        case MouseDrag(point, _, _) :: tail => mousePosition = Some(point)
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseDown(p, MouseButtonLeft, _):: tail => {
          point = Some(p)
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
            unfilteredX = coordinateX
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

      if (coordinateValue.length > 0) {
        val x = if (coordinateX.isDefined) "%.3f" format coordinateX.get
                else coordinateValue
        val y = if (coordinateY.isDefined) "%.3f" format coordinateY.get
                else if (coordinateX.isDefined) coordinateValue
                else ""
        interface display "point (X: "+x+", Y: "+y+")."
      } else if (mousePosition.isDefined) {
        val x = "%.3f" format (if (coordinateX.isDefined) coordinateX.get else mousePosition.get.x)
        val y = "%.3f" format mousePosition.get.y
        interface display "point (X: "+x+", Y: "+y+")."
      } else
        interface display "click or type point"
      //if the next point has been typed, add it to the polyline:

      if (coordinateX.isDefined && coordinateY.isDefined ) {
        //convert the relative coordinates a global point by adding the latest point
        val x = coordinateX.get
        val y = coordinateY.get

        //add the typed point to the polyline
        point = Some(Vector2D(x,y))

        //clear the coordinate vars
        coordinateX = None
        coordinateY = None
        coordinateValue = ""
      }
    }
  ),
    'End -> ((events : List[Event]) => {

      println("ENDING POINT MODULE")



      //Clear the variables
      shape = PolylineShape.empty
      //point = None
      coordinateX = None
      coordinateY = None
      coordinateValue = ""
      if(point.isDefined)
        Message(point.get)
    }
  )
)

  override def paint(g : Graphics, t : TransformationMatrix) {
    // Draw the crosshair
    //if (coordinateX.isDefined) {
    //  val color = new Color(0.2f, 0.2f, 0.2f, 0.65f)
    //  val xToVirtual = Vector2D(x.get, 0).transform(t).x
    //  val screenBottomRight = Siigna.screen.bottomRight
    //  val screenTopLeft     = Siigna.screen.topLeft
      //g draw LineShape(Vector2D(xToVirtual, screenTopLeft.y), Vector2D(xToVirtual, screenBottomRight.y)).addAttributes("Color" -> color)
    //  if (y.isDefined) {
    //   val yToVirtual = Vector2D(0, y.get).transform(t).y
    //    g draw LineShape(Vector2D(screenTopLeft.x, yToVirtual), Vector2D(screenBottomRight.x, yToVirtual)).addAttributes("Color" -> color)
    //    g draw LineShape(Vector2D(xToVirtual, yToVirtual), Vector2D(xToVirtual, yToVirtual)).transform(t)
    //  }
    }

    // Draw a point guide with the new point as a parameter
    //val guide : Vector2D => ImmutableShape = if (pointGuide.isDefined)
    //  pointGuide.get.guide
    //else (previousPoint.isDefined)
    //  LineShape(previousPoint.get, _)
    //else PointShape(_)

    //if (x.isDefined && y.isDefined)
    //  g draw guide(Vector2D(x.get, y.get)).transform(t)
    //else if (x.isDefined && mousePosition.isDefined)
    //  g draw guide(Vector2D(x.get, mousePosition.get.y)).transform(t)
    //else if (mousePosition.isDefined)
    //  g draw guide(mousePosition.get).transform(t)
  }



/**
 * A class used to draw guides in the point module.
 */
case class PointGuide(guide : Vector2D => ImmutableShape)
