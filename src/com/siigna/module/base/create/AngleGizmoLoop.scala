package com.siigna.module.base.create

import com.siigna._
import java.lang.InterruptedException

/**
 * A thread that evaluate the period of time MouseDown is pressed, and bypasses the angle gizmo if the mouse is not
 * held long enough.
 */
class AngleGizmoLoop extends Thread {

  override def run() {
    try {
      // Set the start time
      val startTime = System.currentTimeMillis()

      // Run if the time hasn't expired
      while (System.currentTimeMillis() - startTime < AngleGizmo.gizmoTime) {

        // Interrupt the thread if something bad happens
        if (isInterrupted || !AngleGizmo.isActive) {
          throw new InterruptedException("Angle Gizmo thread stopped.")
        }

        // If the start point is defined
        if (AngleGizmo.startPoint.isDefined) {
          // .. And if the event is not a mouse down
          //TODO: add a mechanism to stop AngleLoop from forwarding "Goto('End)" AFTER AngleGizmo is exited, as this will contaminate the event stream.
          if (!AngleGizmo.latestEvent.get.isInstanceOf[MouseDown]) {
            // Then send the angle gizmo module to end
            Goto('End)

            // And stop the thread
            interrupt()
          }
        }
      }
      // If the time has expired then draw the AngleGizmo - if the latest event is a mouse down.
      if (AngleGizmo.latestEvent.get.isInstanceOf[MouseDown])  {
        Goto ('AngleGizmo)
      }
    } catch {
      case e =>
    }
  }

}