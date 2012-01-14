package com.siigna.module.endogenous

import com.siigna._

/**
 * A thread that evaluate the period of time MouseDown is pressed, and bypasses the angle gizmo if the mouse is not
 * held long enough.
 */
class AngleGizmoLoop extends Thread {

  override def run() {
    val startTime = System.currentTimeMillis()
    while (System.currentTimeMillis() - startTime < AngleGizmo.gizmoTime && AngleGizmo.runGizmo == true) {
      //println(System.currentTimeMillis() - startTime)
      println("gizmo loop. latest event: "+AngleGizmo.latestEvent)
      if (AngleGizmo.latestEvent.isDefined) {
        if (!AngleGizmo.latestEvent.get.isInstanceOf[MouseDown]) {
          println("in angle gizmo loop check. Check failed, do not run gizmo.")
          AngleGizmo.runGizmo = false
          Goto('End)
        }
      }
    }
    if (AngleGizmo.latestEvent.get.isInstanceOf[MouseDown]) {
      Goto ('AngleGizmo)
    }
  }

}