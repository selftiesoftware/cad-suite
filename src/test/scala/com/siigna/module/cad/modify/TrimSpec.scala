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

/**
 * Tests the [[com.siigna.module.cad.modify.Trim]] class.
 */

class TrimSpec extends FunSpec with ShouldMatchers {

  val lineVert = PolylineShape(List(Vector2D(-100,0),Vector2D(10,10), Vector2D(100,0)))
  val lineHoriz1 = PolylineShape(List(Vector2D(-10,20),Vector2D(-15,-30)))
  val lineHoriz2 = PolylineShape(List(Vector2D(10,20),Vector2D(10,-30)))

  //instantiate the trim class
  val trimModule = new Trim

  describe("polylineShapes... ") {
    it("can be trimmed by another polyline") {
      trimModule.trimTwoShapes(lineVert,lineHoriz1) should equal(Set(Vector2D(-11.192660550458712,8.073394495412845)))
    }
  }
}