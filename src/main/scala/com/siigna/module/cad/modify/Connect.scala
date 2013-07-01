/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.modify

import com.siigna.module.Module
import com.siigna._
import util.geom.Geometry2D

/**
 *  A module to connect two line segments. Either as a corner, or with an arc of a given radius.
 */

class Connect extends Module{

  private var attr = Attributes()
  private var connectPoint1 : Option[Vector2D] = None
  private var connectPoint2 : Option[Vector2D] = None
  private var done = false

  private var filletShape : Option[Shape] = None
  private var shape1 : Option[Shape] = None
  private var shape2 : Option[Shape] = None
  private var shapeID1 : Option[Int] = None
  private var shapeID2 : Option[Int] = None

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

  //getIntersections. Returns the segment number (Int) of the intersection for the respective input shapes: 
  //the trimguide, and the trimshapes, respectively
  def getIntersectSegmentNumber(g : Geometry2D, t : Geometry2D) : List[Int] = {
    val gV = g.vertices
    val tV = t.vertices
    var gS = 0
    var tS = 0
    if(gV.length >= 2 && tV.length >= 2) {
      for (i <- 0 to tV.length - 2) {
        for(j <- 0 to gV.length - 2) {
          if(IntersectEval(tV(i),tV(i+1),gV(j),gV(j+1)) == true){
            gS = j
            tS = i
          }
        }
      }
    }
    List(gS, tS)
  }

  /* getSubSegment:
        *----
        | -> second subsegment
        * (d)
       (p)
        | -> first subsegment
     ---*
   evaluate if a point (p) lies on the first or second sub segment of a line (p1,p2) 
   -which has been divided by a point (d). 
   Returns 0 if the point is on the first subsegment.
   Returns 1 if the point is not on any of the segments
   Returns 2 if the point is on the first subsegment.
  */
  def getSubSegment (p1: Vector2D, p2 : Vector2D, d : Vector2D, p : Vector2D) : Int = {
    val dist1 = p.distanceTo(Segment2D(p1,d))
    val dist2 = p.distanceTo(Segment2D(p2,d))

    if(dist1 < dist2) 1
    else if(dist1 > dist2) 2
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

  //evaluates a guideline and a (poly)line returns the polyline, trimmed by the line.
  //TODO: implement fillet
  def connect() {
    println("connect function!")
  }

  val stateMap: StateMap = Map(
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      //if the mouse is pressed, goto selection to evaluate if there are any shapes to be selected:
      case MouseDown(p, MouseButtonLeft, modifier) :: tail => {
        println("GOT MD")
        //get the first shape
        if (Drawing.selection.isEmpty) {
          println("YES")
          Start('cad, "base.Selection", MouseDown(p, MouseButtonLeft, modifier)) //look for one more shape to connect to
        }
        
        //set the first shape
        if(Drawing.selection.size == 1) {
          connectPoint1 = Some(p) //get the location of the first segment to connect.
          shape1 = Some(Drawing.selection.shapes.head._2)
          shapeID1 = Some(Drawing.selection.shapes.head._1)
          attr = shape1.get.attributes //get attributes of the first shape to be transferred to the second.
          Drawing.deselect()
          Start('cad, "base.Selection", MouseDown(p, MouseButtonLeft, modifier)) //look for one more shape to connect to
          
        }
        //set the second shape and connect the two, if possible.  
        else if (Drawing.selection.size == 2) {
          connectPoint2 = Some(p) //get the location of the first segment to connect.
          shape2 = Some(Drawing.selection.shapes.head._2)
          shapeID2 = Some(Drawing.selection.shapes.head._1)

          //do the connection:
          //val l = trim(guideShape, trimShape, v)
          // Create(PolylineShape(l).addAttributes(attr))
          done = true
          //if the shapes have not been set, set them.
        } else {
          None
        } //if the point is not set next to a shape, goto selection and try again
      }
      case _ => {
        println("sel: "+Drawing.selection)
        if(Drawing.selection.size == 1) {
          //connectPoint1 = Some(v) //get the location of the first segment to connect.
          shape1 = Some(Drawing.selection.shapes.head._2)
          shapeID1 = Some(Drawing.selection.shapes.head._1)
          //attr = shape1.get.attributes //get attributes of the first shape to be transferred to the second.
          //Start('cad, "base.Selection") //look for one more shape to connect to
          
        } else println("no selection in ANY!")
      }
    }
  )
}