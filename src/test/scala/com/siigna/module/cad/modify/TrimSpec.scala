/*
 * Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.cad.modify

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.siigna.util.geom._
import com.siigna.app.model.shape.{PolylineLineShape, PolylineShape}
import com.siigna.util.collection.Attributes
import scala.collection.mutable.ArrayBuffer
import com.siigna.app.model.shape.PolylineShape.PolylineShapeOpen

/**
 * Tests the [[com.siigna.module.cad.modify.Trim]] class.
 */

class TrimSpec extends FunSpec with ShouldMatchers {

  val attr = Attributes()
  val gs1 = PolylineShape(List(Vector2D(-10,20),Vector2D(-15,-30)))
  val gs2 = PolylineShape(List(Vector2D(10,20),Vector2D(10,-30)))
  val trimLine = PolylineShape(List(Vector2D(-100,0),Vector2D(10,10), Vector2D(100,0)))
  val twoGuides = List(gs1, gs2)

  val p1 = Vector2D(55,5)
  val p2 = Vector2D(0,9.09175)
  val p3 = Vector2D(-50,4.54321)
  val p4 = Vector2D(-50,0)

  //TODO: test trimming of a rotated rect inside a larger rect, creating four intersections:

  describe("intersections... ") {

    it("can be found with a PL intersected twice by two PolyLines") {
      TrimmingMethods.getIntersectSegmentNumbers(twoGuides,trimLine) should equal(Map(0 -> List(Vector2D(-11.192660550458715,8.073394495412844), Vector2D(10.0,10.0)), 1 -> List(Vector2D(10.0,10.0))))
    }

    //findIntersection tests:

    it("can return the segment ID and innerShape on which a point is set") {
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p1) should equal (Some((1,PolylineLineShape(Vector2D(100.0,0.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p2) should equal (Some((0,PolylineLineShape(Vector2D(10.0,10.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p3) should equal (Some((0,PolylineLineShape(Vector2D(10.0,10.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p4) should equal (None)
    }

    //it("can return the first relevant intersection (to be used for trimming) in a given direction from the trim point ") {
    //  val ints = TrimmingMethods.getIntersectSegmentNumbers(twoGuides,trimLine)
    //  TrimmingMethods.findIntersection(trimLine,1,ints,true,p1) should equal(None)
    //}

    //it("can be trimmed by another PolylineShape when there is one guideShape") {
    //  TrimmingMethods.trimPolyline(twoGuides,trimLine,p1) should equal(PolylineShape(Vector2D(-100,0),Vector2D(-11.192660550458712,8.073394495412845)))
    //}


    //it("can be trimmed by two GuideShapes (polylines) when the trim point is between the two GuideShapes") {

    //  trimModule.trimTwoPolyLineShapes(twoGuides,trimLine,p2) should equal(Set(PolylineShape(Vector2D(-100,0),Vector2D(-11.192660550458712,8.073394495412845)),PolylineShape(Vector2D(10,10),Vector2D(100,0))))
  //}
  }

}