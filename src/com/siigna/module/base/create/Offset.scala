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

class Offset extends Module {
  var attr = Attributes()
  var done = false
  //a function to offset a line segment
  def calcOffset(s : LineShape, dist : Double) : LineShape = {
    val lT = s.transform(TransformationMatrix(-s.p1,1)) //move the segment to 0,0
    val length : Int = ((s.p2 - s.p1).length).toInt //get the length of the segment
    val lS = lT.transform(TransformationMatrix.apply(Vector2D(0,0),1/length.toDouble))//scale it to the unit vector
    val lR = lS.transform(TransformationMatrix().rotate(90, Vector2D(0,0))) //rotate it 90 degrees
    val pt = if(lR.p1 == Vector2D(0,0)) lR.p2 else lR.p1  //get the point on the vector which is not on (0,0)
    val offsetDirection = -pt * dist //get the length of the offsetvector
    val offsetGeom = s.transform(TransformationMatrix(offsetDirection,1))//offset with the current distance
    offsetGeom
  }
  //returns the intersecting points of a series of line segments.
  def getKnots(l : List[LineShape]) = {
    var k =  List[Vector2D]()
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
  //calculate on which side of a given line segment the offset should be.
  def offsetSide (s: LineShape, m : Vector2D) : Boolean = {
    val angleRaw = ((s.p2-s.p1).angle * -1) + 450
    val angle = if(angleRaw > 360) angleRaw - 360 else angleRaw
    val linePt = s.geometry.closestPoint(m)
    val ly = linePt.y
    val my = m.y
    //if the mouse is north of the shape, offset should extend upwards
    if(angle > 0 && angle < 180) {
      if(my > ly) false
      else true
    }
    //if the mouse is south of the shape, offset should extend downwards
    else {
      if(my < ly) false
      else true
    }
  }

  //calculates the distance to offset shapes by, then calls the offset function to returns offset shapes as a list
  def offsetLines(s : Shape, m : Vector2D) = {
    var l = List[LineShape]()
    var v = s.geometry.vertices
    //iterate through the shapes to find the shape closest to the mouse
    def calcNearest : Double = {
      var nearestDist : Double = LineShape(v(0), v(1)).distanceTo(m)
      var closestSegment : Option[LineShape] = Some(LineShape(v(0), v(1)))
      for (i <- 0 to v.length -2) {
        var currentSegment = LineShape(v(i), v(i+1))
        if (currentSegment.distanceTo(m) < nearestDist) {
          closestSegment = Some(currentSegment)
          nearestDist = currentSegment.distanceTo(m)
        }
      }
      //check on which side of the original the offset should take place
      val n = if(closestSegment.isDefined && offsetSide(closestSegment.get, m) == true) nearestDist
      else  - nearestDist
      n
    }
    val distance : Double = calcNearest
    //run the offset function to offset shapes with the distance set in calcNearest
    for (i <- 0 to v.length-2) l = l :+ calcOffset(LineShape(v(i), v(i+1)),distance)
    l//return the list
  }

  def offsetLines(s : Shape, m : Double) = {
    var l = List[LineShape]()
    var v = s.geometry.vertices
    //iterate through the shapes to find the shape closest to the mouse
    def calcNearest : Double = {
      var nearestDist : Double = LineShape(v(0), v(1)).distanceTo(mousePosition)
      var closestSegment : Option[LineShape] = Some(LineShape(v(0), v(1)))
      for (i <- 0 to v.length -2) {
        var currentSegment = LineShape(v(i), v(i+1))
        if (currentSegment.distanceTo(mousePosition) < nearestDist) {
          closestSegment = Some(currentSegment)
          nearestDist = currentSegment.distanceTo(mousePosition)
        }
      }
      //check on which side of the original the offset should take place
      val n = if(closestSegment.isDefined && offsetSide(closestSegment.get, mousePosition) == true) m
      else  - m
      n
    }

    val distance : Double = calcNearest
    //run the offset function to offset shapes with the distance set in calcNearest
    for (i <- 0 to v.length-2) l = l :+ calcOffset(LineShape(v(i), v(i+1)),distance)
    l//return the list
  }

  //a guide to get Point to draw the shape(s) dynamically
  val guide: PointGuide = PointGuide((v : Vector2D) => {
    val shape = Drawing.selection.head.shapes.head._2
    //HACK - to be able to use the point guide to dynamicaly draw key-entered offset distance:
    var newLines = List[LineShape]()
    if (v.y == 12345.6789) {
      newLines = offsetLines(shape, v.x) //offset the lines by the current double
    } else {
      newLines = offsetLines(shape, v) //offset the lines by the current mouseposition
    }
    var knots = List[Vector2D]()

    knots = knots :+ newLines.head.p1 //add the first point to the list

    def result = getKnots(newLines).foreach(s => knots = knots :+ s) //add the intersections to the konts list
    result
    knots = knots :+ newLines.reverse.head.p2 //add the last vertex
    Array(PolylineShape(knots).addAttributes(attr))//create a polylineShape from the offset knots:
  },131)// 131: MouseDown or typed length - special guide in InputOneValue

  //Select shapes
  val stateMap: StateMap = Map(

  'Start -> {
    case End(p : Vector2D) :: tail => {
      val shape = Drawing.selection.head.shapes.head._2
      val newLines = offsetLines(shape, p) //offset the lines by the returned point
      var knots = List[Vector2D]()

      knots = knots :+ newLines.head.p1 //add the first point to the list

      def result = getKnots(newLines).foreach(s => knots = knots :+ s) //add the intersections to the konts list
      result
      knots = knots :+ newLines.reverse.head.p2 //add the last vertex
      done = true
      Create(PolylineShape(knots).addAttributes(attr))//create a polylineShape from the offset knots:
      End
    }

    case End(d : Double) :: tail => {
      val shape = Drawing.selection.head.shapes.head._2
      val newLines = offsetLines(shape, d) //offset the lines by the returned double
      var knots = List[Vector2D]()

      knots = knots :+ newLines.head.p1 //add the first point to the list

      def result = getKnots(newLines).foreach(s => knots = knots :+ s) //add the intersections to the konts list
      result
      knots = knots :+ newLines.reverse.head.p2 //add the last vertex
      done = true
      Create(PolylineShape(knots).addAttributes(attr))//create a polylineShape from the offset knots:
      End
    }

    case MouseUp(_, MouseButtonRight, _) :: tail => End

    //exit strategy
    case KeyDown(Key.Esc, _) :: tail => End
    case MouseDown(p, MouseButtonRight, _) :: tail => End

    case _ => {
      if (!Drawing.selection.isDefined && done == false) {
        Siigna display "select an object to offset first"
        End
      }
      else if (Drawing.selection.isDefined && Drawing.selection.get.size == 1 ){
        attr = Drawing.selection.head.shapes.head._2.attributes
        Siigna display "click to set the offset distance"
        Start('Input,"com.siigna.module.base.create", guide)
      }
      else if (done == true) End
      else {
        Siigna display "please select one shape to offset"
        Drawing.deselect()
        End
      }
    }
  })
}

