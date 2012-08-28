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
import module.base.create.PointGuide._

object Offset extends Module {

  var distancePoint : Option[Vector2D] = None

  val eventHandler = EventHandler(stateMap, stateMachine)

  var originals : Option[Selection] = None

  //a guide to get Point to draw the shape(s) dynamically
  val shapeGuide : Vector2D => Traversable[Shape] = (v : Vector2D) => {
    val s = Drawing.selection.get
    //get a point on the shape perpendicular to the mouse position
    val p = s.shapes.get(1)

    println(p)
    // Create a matrix
    val t : TransformationMatrix = TransformationMatrix(v, 1)
    // Return the shape, transformed
    s.apply(t)
  }
  
  var text  = ""

  def stateMap = DirectedGraph(
    'Start         -> 'Message ->         'SetDistance
  )

  //Select shapes
  def stateMachine = Map(
  'Start -> ((events : List[Event]) => {
    //println(events)
    if (!Drawing.selection.isDefined) {
      println("C")
      Siigna display "select an object to offset"
      ForwardTo('Selection)
    }
    else if (Drawing.selection.isDefined && Drawing.selection.get.size == 1 && !distancePoint.isDefined){
      Siigna display "click to set the offset distance"
      println("goint to point")
      //TODO: causes SetDistance to run before a point is set.
      //Controller ! Message(PointGuides(shapeGuide))
      ForwardTo('Point, false)
    }
  }),
  'SetDistance -> ((events : List[Event]) => {
    events match {
      // Exit strategy
      case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

      case Message(p : Vector2D) :: tail => {
        println("A")
        distancePoint = Some(p)
        Goto('End)
      }
      case _ => Goto('End)
    }
  }),

    //do the offset calculation here
    'End -> ((events : List[Event]) => {
      //get the point on the line that is perpendicular to the distance point
      if(distancePoint.isDefined) {
        val transformation : TransformationMatrix = TransformationMatrix(distancePoint.get, 1)
        Drawing.selection.get.apply(transformation)
      }

      println("END")
      println("offset here with distance: "+distancePoint)
      Drawing.deselect()
  }))
}
