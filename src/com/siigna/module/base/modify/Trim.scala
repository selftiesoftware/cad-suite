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


package com.siigna.module.base.modify

import com.siigna._

class Trim extends Module {

  var nearestShape : Option[Shape] = None

  var selection = List[Vector2D]()
  var shapes : List[Shape] = List()

  var trimGuide : Option[Shape] = None
  var trimShapes : Iterable[Shape] = List()

  var selectionBoxStart : Option[Vector2D] = None
  var selectionBoxEnd : Option[Vector2D] = None

  //returns the intersecting points of a series of line segments.
  def getKnots(l : List[LineShape]) = {
    var k = List[Vector2D]()
    for (i <- 0 to l.length -2) {
      val s1 = l(i)
      val s2 = l(i+1)
      val l1 = Line2D(s1.p1,s1.p2)
      val l2 = Line2D(s2.p1,s2.p2)
      val int = l1.intersections(l2).head
      k = k :+ int
    }
    k
  }

  val stateMap: StateMap = Map(

  'Start -> {
    //exit strategy
    case KeyDown(Key.Esc, _) :: tail => End
    case MouseDown(p, MouseButtonRight, _) :: tail => End

    //if selection returns a point, evaluate if there areany shapes to trim at that point:
    case End(v : Vector2D) :: tail => {
      if(trimGuide.isDefined ) {
        val m = mousePosition.transform(View.deviceTransformation)
        //find the shape closest to the mouse:
        if (Drawing(m).size > 0) {
          val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
          nearestShape = if (nearest._2.distanceTo(m) < Siigna.selectionDistance) Some(nearest._2) else None
          println("got shape: "+nearestShape.get)
          
          val trimShapes = nearestShape.get.geometry
          val guideShape = trimGuide.get.geometry
          //do the trimming:
          //val intersections = trimShapes.intersections(guideShape)
          //println("intersections: "+intersections)

        } else {
          println("got no shape. trying again.")
          Start('Input,"com.siigna.module.base.create",1)
        } //if the point is not set next to a shape, goto selection and try again
      }
      Track.trackEnabled = true

    }
    case _ => {
      Track.trackEnabled = false
      if (Drawing.selection.isDefined && Drawing.selection.get.size == 1) {
        trimGuide = Some(Drawing.selection.head.shapes.head._2)
        Siigna.display("Select shapes to trim")
        Start('Input,"com.siigna.module.base.create",1)

      } else {
        Siigna.display("Select an object to trim objects by")
        Start('Input,"com.siigna.module.base.create",1)
      }
    }
  })
}
