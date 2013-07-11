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
import com.siigna.app.model.shape.{LineShape, PolylineLineShape, PolylineShape}
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
  val gs3 = PolylineShape(List(Vector2D(20,20),Vector2D(20,-30)))
  val gs4 = PolylineShape(List(Vector2D(30,20),Vector2D(40,-30)))
  val gs5 = PolylineShape(List(Vector2D(60,20),Vector2D(60,-30)))
  val trimLine = PolylineShape(List(Vector2D(-100,0),Vector2D(10,10), Vector2D(100,0)))
  val twoGuides = List(gs1, gs2)
  val fiveGuides = List(gs1, gs2, gs3, gs4, gs5)
  val twoGuidesOnSegment = List(gs1, gs3)

  val p1 = Vector2D(55,5)
  val p2 = Vector2D(0,9.09175)
  val p3 = Vector2D(-50,4.54321)
  val p4 = Vector2D(0,-50)

  //TODO: test trimming of a rotated rect inside a larger rect, creating four intersections:

  describe("intersections on a PL... ") {

    it("can be found with a PL intersected twice by two PolyLines") {
      TrimmingMethods.getIntersectSegmentNumbers(twoGuides,trimLine) should equal(Map(0 -> List(Vector2D(-11.192660550458715,8.073394495412844), Vector2D(10.0,10.0)), 1 -> List(Vector2D(10.0,10.0))))
    }

    //findIntersection tests:

    it("can return the segment ID and Segment on which a point is set") {
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p1) should equal (Some((1,Segment2D(Vector2D(10.0,10.0),Vector2D(100.0,0.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p2) should equal (Some((0,Segment2D(Vector2D(-100.0,0.0),Vector2D(10.0,10.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p3) should equal (Some((0,Segment2D(Vector2D(-100.0,0.0),Vector2D(10.0,10.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p4) should equal (None)
    }

    it("can return the first relevant intersection (to be used for trimming) in a given direction from the trim point ") {
      val oneInt = TrimmingMethods.getIntersectSegmentNumbers(List(gs1),trimLine)
      val twoInts = TrimmingMethods.getIntersectSegmentNumbers(twoGuidesOnSegment,trimLine)
      //one of the guide lines is on top of a vertex:
      val intsTlOnNode = TrimmingMethods.getIntersectSegmentNumbers(twoGuides,trimLine)
      val fiveInts = TrimmingMethods.getIntersectSegmentNumbers(fiveGuides,trimLine)

      TrimmingMethods.findIntersection(trimLine,intsTlOnNode,true,p1) should equal(None) //positive direction
      TrimmingMethods.findIntersection(trimLine,twoInts,false,p1) should equal(Some(Vector2D(20.0,8.88888888888889))) //negative direction
      TrimmingMethods.findIntersection(trimLine,intsTlOnNode,false,p1) should equal(Some(Vector2D(10.0,10.0))) //negative direction
      TrimmingMethods.findIntersection(trimLine,fiveInts,true,p1) should equal (Some(Vector2D(60.0,4.444444444444445))) //positive direction
      TrimmingMethods.findIntersection(trimLine,fiveInts,false,p1) should equal (Some(Vector2D(32.5,7.5))) //negative direction

      //situations where intersections occur on adjacent segments only:
      //TrimmingMethods.findIntersection(trimLine,oneInt,false,p1) should equal (Some(Vector2D(-11.192660550458715,8.073394495412844)))
    }

    //it("can be trimmed by another PolylineShape when there is one guideShape") {
    //  TrimmingMethods.trimPolyline(twoGuides,trimLine,p1) should equal(PolylineShape(Vector2D(-100,0),Vector2D(-11.192660550458712,8.073394495412845)))
    //}


    //it("can be trimmed by two GuideShapes (polylines) when the trim point is between the two GuideShapes") {

    //  trimModule.trimTwoPolyLineShapes(twoGuides,trimLine,p2) should equal(Set(PolylineShape(Vector2D(-100,0),Vector2D(-11.192660550458712,8.073394495412845)),PolylineShape(Vector2D(10,10),Vector2D(100,0))))
  //}
  }

}