/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  // The AngleGuide is the guide that comes from the AngleGizmo
  private var angleGuide : Option[Double] = None

  // The anglePoint is the point where the angle gizmo is centered, and thus
  // the point where a possible future angleGuide has to extend from
  private var anglePoint : Option[Vector2D] = None

  // Store the mousePosition, so we get the snap-coordinates
  private var mousePosition : Option[Vector2D] = None

  // The points of the polyline
  private var points   = List[Vector2D]()

  // The polylineshape so far
  private var shape : PolylineShape = PolylineShape.empty

  // Preload AngleGizmo
  Preload('AngleGizmo, "com.siigna.module.endogenous.AngleGizmo")

  val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseMove(point, _, _) :: tail => mousePosition = Some(point)
        case MouseDrag(point, _, _) :: tail => mousePosition = Some(point)
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseDown(point, MouseButtonLeft, _):: tail => {
          // Add the point
          points = points :+ point

          // Stop snapping
          eventParser.clearSnap()

          // Set the polyline from the points saved in shape
          shape = PolylineShape.fromPoints(points)

          // Set the angle point
          anglePoint = Some(Siigna.mousePosition)

          // Forward to angle gizmo
          ForwardTo('AngleGizmo)
        }
        case Message(p : Double) :: tail => {
          if (anglePoint.isDefined) {
            angleGuide = Some(p)

            // Since we got the angle we can now snap to the center point and the angle
            eventParser.snapTo(new AngleSnap(anglePoint.get, p))
          }
        }
        case MouseUp(_, MouseButtonRight, _):: tail => Goto ('End)
        case KeyDown(Key.Enter, _) :: tail => Goto ('End)
        case KeyUp(Key.Space, _) :: tail => Goto ('End)
        case _ =>
      }
    }),
    'End -> ((events : List[Event]) => {
      // Create the final polyline
      Create(shape)

      // Clear the points list
      points = List[Vector2D]()
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0) {
      g draw shape.transform(t)
      //draw a the current mouse position, transformed by the active radian if the angle gizmo is active
      if (mousePosition.isDefined)
        g draw LineShape(mousePosition.get, points.last).transform(t)
    }
  }
}