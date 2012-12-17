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
import util.geom.Geometry2D

class Trim extends Module {

  var nearestShape : Option[Shape] = None

  var selection = List[Vector2D]()
  var shapes : List[Shape] = List()

  var trimGuide : Option[Shape] = None
  var trimShapes : Iterable[Shape] = List()

  var selectionBoxStart : Option[Vector2D] = None
  var selectionBoxEnd : Option[Vector2D] = None

  def IsOnSegment(pt1 : Vector2D, pt2 : Vector2D, pt3 : Vector2D) = {
    val xi = pt1.x
    val yi = pt1.y
    val xj = pt2.x
    val yj = pt2.y
    val xk = pt3.x
    val yk = pt3.y
    if((xi <= xk || xj <= xk) && (xk <= xi || xk <= xj) && (yi <= yk || yj <= yk) && (yk <= yi || yk <= yj)) {
      true
    } else false
  }

  def ComputeDirection(pt1 : Vector2D, pt2 : Vector2D, pt3 : Vector2D) = {
    val xi = pt1.x
    val yi = pt1.y
    val xj = pt2.x
    val yj = pt2.y
    val xk = pt3.x
    val yk = pt3.y
    val a = (xk - xi) * (yj - yi)
    val b = (xj - xi) * (yk - yi)
    //return (a < b) ? -1 : a > b ? 1 : 0
    if(a < b) -1
    else if(a > b) 1
    else 0
  }

  /** Do line segments (x1, y1)--(x2, y2) and (x3, y3)--(x4, y4) intersect? */
  def IntersectEval(pt1 : Vector2D, pt2 : Vector2D, pt3 : Vector2D,  pt4 : Vector2D) : Boolean = {
    val d1 = ComputeDirection(pt3, pt4, pt1)
    val d2 = ComputeDirection(pt3, pt4, pt2)
    val d3 = ComputeDirection(pt1, pt2, pt3)
    val d4 = ComputeDirection(pt1, pt2, pt4)    
    var intersects = false

    if(((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0))) intersects = true
    if((d1 == 0 && IsOnSegment(pt3, pt4, pt1)) || (d2 == 0 && IsOnSegment(pt3, pt4, pt2)) || (d3 == 0 && IsOnSegment(pt1, pt2, pt3)) || (d4 == 0 && IsOnSegment(pt1, pt2, pt4))) intersects = true
  intersects
  }


  //calculate on which side of a given line segment the trim should be.
  def trimSide (l: Line2D, m : Vector2D) : Boolean = {
    val s = LineShape(l.p1,l.p2)
    val angleRaw = ((s.p2-s.p1).angle * -1) + 450
    //reformat the angles to lie between 0 and 180
    val angle1 = if(angleRaw > 360) angleRaw - 360 else angleRaw
    val angle = if(angle1 > 180) angle1 - 180 else angle1 //
    val linePt = s.geometry.closestPoint(m)//get a point on the line to evaluate against
    val ly = linePt.y
    val my = m.y
    println("ANGLE: "+angle)
    //if the mouse is north of the shape, offset should extend upwards
    if(angle > 0 && angle < 90) {
      if(my > ly) true
      else false
    }
    //if the mouse is south of the shape, offset should extend downwards
    else {
      if(my < ly) false
      else true
    }
  }
  //a function that splits a shape into two lists by a point
  def splitPolyline(p : List[Vector2D], splitPoint : Vector2D) : (List[Vector2D], List[Vector2D]) = {
    var newP = p //reverse the list if the drawing order requires it.
    var list1 = List[Vector2D]()
    var list2 = List[Vector2D]()
    //evaluate on which segment the splitpoint lies
    var r = 0
    if(p.length >= 2) {
      for(i <- 0 to (p.length - 2)) {
        if(IsOnSegment(p(i),p(i+1),splitPoint) == true) {
          r = i
          //check the drawing order: 
          if(p(i).y < splitPoint.y) newP = p.reverse
          //reverse the list list p is neccesary
        }
      }
    }
    //make a list of points up until the split segment
    for (i <- 0 to r) list1 = list1 :+ newP(i)
    //make a list of points after the split segment
    for (i <- r to p.length - 2) list2 = list2 :+ newP(i)
    
    val drawOrder : Boolean = {
      if(list1.head.y > splitPoint.y)false
      else true
    }
    if(drawOrder == true) {
       println("split draworder, RIGHT")
      (list1, list2)
    }
    else {
      println("split draworder, LEFT")
      (list2, list1)
    }

  }

  //evaluates a guideline and a (poly)line returns the polyline, trimmed by the line.
  //TODO: allow trimming of lines that intersect the guide multiple times
  def trim(guide : Geometry2D, trimLine : Geometry2D, p : Vector2D) : List[Vector2D] = {
    val g = guide.vertices
    val t : Seq[Vector2D] = trimLine.vertices
    var trimV = List[Vector2D]()
    //make a line segment of the guide line. //TODO: allow polylines to be guide objects
    val gLine = Line2D(g(0),g(1))

    for (i <- 0 to t.length - 2) {
      val l1 = Line2D(t(i),t(i+1)) //create a line segment to evaluate
      //exclude intersections that happen in the extension of the guide segment
      val isIntersecting : Boolean = IntersectEval(t(i),t(i+1),g(0),g(1))
      //if the guide and the segment intersects, get the intersection.
      val int = {
        if(isIntersecting == true) l1.intersections(gLine)
        else Seq()
      }
      //test on what side of the intersection the trimming should take place
      //split the (poly)line at the trim segment
      val trimmedLines = if(!int.isEmpty) splitPolyline(t.toList,int.head) else (List[Vector2D](), List[Vector2D]())

      if(!int.isEmpty && trimSide(gLine, p) == false) {
        //TODO: the segments are not set correctly here...
        println("LEFT: "+trimmedLines._2)
        trimV = trimmedLines._2 //add the points before the trim
        trimV = trimV :+ int.head  //add the new intersection point instead of the second point
      } else if (!int.isEmpty && trimSide (gLine, p) == true){
        println("RIGHT: "+trimmedLines._1)
        trimV = trimmedLines._1.reverse.take(trimmedLines._2.length - 1) //add the points after the trim
        trimV = trimV :+ int.head  //add the new intersection point instead of the second point
      }
    }
    trimV
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

          val trimShapes = nearestShape.get.geometry
          val guideShape = trimGuide.get.geometry

          //do the trimming:
          Delete(nearest._1)
          val l = trim(guideShape, trimShapes, v)
          Create(PolylineShape(l))
          End

        } else {
          println("got no shape. trying again.")
          Start('Input,"com.siigna.module.base.create",1)
        } //if the point is not set next to a shape, goto selection and try again
      }
      Track.trackEnabled = true
      End
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
