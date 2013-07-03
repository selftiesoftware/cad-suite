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
import com.siigna.app.model.shape.PolylineShape
import com.siigna.util.collection.Attributes

/**
 * Tests the [[com.siigna.module.cad.modify.Trim]] class.
 */

class TrimSpec extends FunSpec with ShouldMatchers {

  val attr = Attributes()
  val gs1 = PolylineShape(List(Vector2D(-10,20),Vector2D(-15,-30)))
  val gs2 = PolylineShape(List(Vector2D(10,20),Vector2D(10,-30)))
  val oneGuide = List(gs1)
  val trimLine = PolylineShape(List(Vector2D(-100,0),Vector2D(10,10), Vector2D(100,0)))
  val twoGuides = List(gs1, gs2)

  val p1 = Vector2D(55,5)
  val p2 = Vector2D(0,9.09175)
  val p3 = Vector2D(-50,4.54321)


  describe("a PolylineShape... ") {
    it("can be trimmed by another PolylineShape when the trimpoint is next to the endpoint") {
      //TrimmingMethods.trimTwoPolyLineShapes(oneGuide,trimLine,p1,attr) should equal(Set(PolylineShape(Vector2D(-100,0),Vector2D(10,10))))
    }


    //it("can be trimmed by two GuideShapes (polylines) when the trim point is between the two GuideShapes") {

    //  trimModule.trimTwoPolyLineShapes(twoGuides,trimLine,p2) should equal(Set(PolylineShape(Vector2D(-100,0),Vector2D(-11.192660550458712,8.073394495412845)),PolylineShape(Vector2D(10,10),Vector2D(100,0))))
  //}
  }

}