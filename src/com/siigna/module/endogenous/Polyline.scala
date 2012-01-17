/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

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

  //variable used to get an offset from the previous point
  def difference : Vector2D = if (previousPoint.isDefined) previousPoint.get else Vector2D(0, 0)

  // Store the mousePosition, so we get the snap-coordinates
  private var mousePosition : Option[Vector2D] = None

  // The points of the polyline
  private var points   = List[Vector2D]()

  private var previousPoint : Option[Vector2D] = None

  // The polylineshape so far
  private var shape : PolylineShape = PolylineShape.empty

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

  // Preload AngleGizmo
  //Preload('AngleGizmo, "com.siigna.module.endogenous.AngleGizmo")


  val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //println("latest event ST: "+events.head)
      events match {
        case MouseMove(point, _, _) :: tail => mousePosition = Some(point)
        case MouseDrag(point, _, _) :: tail => mousePosition = Some(point)
        case MouseUp(_, MouseButtonRight, _) :: tail => {
          Goto('End)
        }
        case MouseDown(point, MouseButtonLeft, _):: tail => {
          //println("latest event MD: "+events.head)

          //add the point set to the polylineShape
          points = points :+ point

          //and store it in a var.
          previousPoint = Some(point)

          // Stop snapping
          //eventParser.clearSnap()

          // Set the angle point
          //anglePoint = Some(Siigna.mousePosition)

          // Forward to angle gizmo
          //ForwardTo('AngleGizmo)
        }
        //case Message(p : Double) :: tail => {
        //  if (anglePoint.isDefined) {
        //    angleGuide = Some(p)

            // Since we got the angle we can now snap to the center point and the angle
        //    eventParser.snapTo(new AngleSnap(anglePoint.get, p))
        //  }
        //}
        case MouseUp(_, MouseButtonRight, _):: tail => {
          Goto ('End)
        }
        case KeyDown(Key.Backspace, _) :: tail => {
          if (coordinateValue.length > 0) coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
          else if (coordinateX.isDefined) {
            coordinateValue = coordinateX.get.toString
            coordinateX     = None
          }
        }
        //goto second cooridinate if ENTER or TAB is pressed
        case KeyDown(Key.Enter | Key.Tab | ',', _) :: tail => {
          if (coordinateX.isEmpty && coordinateValue.length == 0) Goto('End)
          if (coordinateX.isEmpty && coordinateValue.length > 0) {
            coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))
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
      if (Model.isEmpty && previousPoint.isEmpty && shape.isEmpty)
        interface display "Click to define starting point."
      else {
        if (coordinateValue.length > 0) {
          val x = if (coordinateX.isDefined) "%.3f" format coordinateX.get
                  else coordinateValue
          val y = if (coordinateY.isDefined) "%.3f" format coordinateY.get
                  else if (coordinateX.isDefined) coordinateValue
                  else ""
          interface display "Enter coordinate (X: "+x+", Y: "+y+")."
        } else if (mousePosition.isDefined) {
          val x = "%.3f" format (if (coordinateX.isDefined) coordinateX.get else mousePosition.get.x) - difference.x
          val y = "%.3f" format mousePosition.get.y - difference.y
          interface display "Click to define coordinate (X: "+x+", Y: "+y+")."
        } else
          interface display "Enter (X, Y) or click to define coordinate."
        //if the next point has been typed, add it to the polyline:
        if (coordinateX.isDefined && coordinateY.isDefined ) {
          val x = coordinateX.get + difference.x
          val y = coordinateY.get + difference.y
          println("X coord =: "+x)
          println("Y coord =: "+y)

          //add the typed point to the polyline
          points = points :+ (Vector2D(x,y))

          //and store it in a var
          println("PP before: "+previousPoint)
          previousPoint = Some(Vector2D(x,y))
          println("PP after: "+previousPoint)
          //clear the coordinate vars
          coordinateX = None
          coordinateY = None
          coordinateValue = ""
        }
      }
      // save a PolylineShape from the points saved in shape
      shape = PolylineShape.fromPoints(points)
    }),
    'End -> ((events : List[Event]) => {

      Create(shape)

      //Clear the variables
      shape = PolylineShape.empty
      points = List[Vector2D]()
      previousPoint = None
      coordinateX = None
      coordinateY = None
      coordinateValue = ""
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0 && previousPoint.isDefined) {
      //draw a the current mouse position, transformed by the active radian if the angle gizmo is active
      if (x.isDefined && y.isDefined)
        g draw LineShape(Vector2D(x.get, y.get), previousPoint.get).transform(t)
      else if (x.isDefined && mousePosition.isDefined)
        g draw LineShape(Vector2D(x.get, mousePosition.get.y), previousPoint.get).transform(t)
      else if (mousePosition.isDefined)
        g draw LineShape(mousePosition.get, previousPoint.get).transform(t)
    g draw shape.transform(t)
    }
  }
}