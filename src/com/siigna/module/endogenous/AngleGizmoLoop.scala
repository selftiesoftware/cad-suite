package com.siigna.module.endogenous

import com.siigna._

/**
 * A thread.
 */
class AngleGizmoLoop extends Thread {

  override def run() {
    val startTime = System.currentTimeMillis()
    
    while (System.currentTimeMillis() - startTime < AngleGizmo.gizmoTime && AngleGizmo.runGizmo) {
      if (AngleGizmo.latestEvent.isDefined) {
        if (!AngleGizmo.latestEvent.get.isInstanceOf[MouseDown]) {
          AngleGizmo.runGizmo = false
          Goto('End)
        } 
      }
    }
  }

}