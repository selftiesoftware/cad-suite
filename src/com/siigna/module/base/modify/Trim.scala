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
import java.awt.Color

class Trim extends Module {

  var attr = Attributes()
  var nearestShape : Option[Shape] = None
  var selection = List[Vector2D]()
  var shapes : List[Shape] = List()
  var selectionBoxStart : Option[Vector2D] = None
  var selectionBoxEnd : Option[Vector2D] = None
  var trimGuide : Option[Shape] = None
  var trimShapes : Iterable[Shape] = List()
  var trimID : Option[Int] = None

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

  //a function that splits a shape into two lists by a point
  def splitPolyline(p : List[Vector2D], splitPoint : Vector2D) : (List[Vector2D], List[Vector2D]) = {
    var list1 = List[Vector2D]()
    var list2 = List[Vector2D]()
    var r = 0
    //evaluate on which segment the splitpoint lies -unless the length is two, in which case it is not a polyline
    if(p.length >= 2) for(i <- 0 to (p.length - 2)) if(IsOnSegment(p(i),p(i+1),splitPoint) == true) r = i

    for (i <- 0 to r) list1 = list1 :+ p(i) //make a list of points up until the split segment
    for (i <- (r + 1) to p.length - 2) list2 = list2 :+ p(i) //make a list of points after the split segment
    (list2, list1)  //return the two lists
  }
  
  //evaluates a guideline and a (poly)line returns the polyline, trimmed by the line.
  //TODO: allow trimming of lines that intersect the guide multiple times
  def trim(guide : Geometry2D, trimLine : Geometry2D, p : Vector2D) : List[Vector2D] = {
    val g : Seq[Vector2D] = guide.vertices
    val t : Seq[Vector2D] = trimLine.vertices
    var trimV = List[Vector2D]()
    val intSegments : List[Int] = getIntersectSegmentNumber(guide,trimLine) //find segment numbers for intersections
    //store return values from getIntersectSegmentNumber
    val guideSegmentNr = intSegments(0)
    val trimSegmentNr = intSegments(1)
    val guideSegment = Line2D(g(guideSegmentNr),g(guideSegmentNr+1))
    val trimSegment = Line2D(t(trimSegmentNr),t(trimSegmentNr+1))   
    val int = guideSegment.intersections(trimSegment)   //do the intersection!
    if (!int.isEmpty) {
      val getSegment = (getSubSegment(t(trimSegmentNr),t(trimSegmentNr+1),int.head,p))
      val twoLists = splitPolyline(t.toList,int.head)//the trimline split into two lists

      if(getSegment == 1) { //if the trim point is set before the trimline, keep the first segments.
        trimV = (twoLists._1 :+ t.last).reverse :+ int.head
      }
      else if(getSegment == 2) { //if the trim point is set after the trimline, keep the last segments.
        trimV = twoLists._2 :+ int.head
      } else trimV = trimLine.vertices.toList
    } //else trimV = t.toList //if not intersection is found, return the original trimLine
    trimV //return the trimmed line
  }

  val stateMap: StateMap = Map(
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => {
        Drawing.deselect()
        End
      }
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      //if selection returns a point, evaluate if there areany shapes to trim at that point:
      case End(v : Vector2D) :: tail => {
        if(trimID.isDefined) Drawing.select(trimID.get) //keep the trimline selected for better visual reference

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
            Create(PolylineShape(l).addAttributes(attr))
            nearestShape = None //reset var
            Start('Input,"com.siigna.module.base.create",1) //look for more trim points
          } else {
            Start('Input,"com.siigna.module.base.create",1)
          } //if the point is not set next to a shape, goto selection and try again
        } else {
        Track.trackEnabled = true
        End
        }
      }
      case _ => {
        Track.trackEnabled = false
        if (Drawing.selection.isDefined && Drawing.selection.get.size == 1) {
          attr = Drawing.selection.head.shapes.head._2.attributes
          trimGuide = Some(Drawing.selection.head.shapes.head._2)
          trimID = Some(Drawing.selection.head.shapes.head._1)
          Siigna.display("Select shapes to trim")
          Start('Input,"com.siigna.module.base.create",1)

        } else {
          //Siigna.display("Select an object to trim objects by")
          End
        }
      }
    })
  override def paint(g : Graphics, t : TransformationMatrix) {
    if(trimGuide.isDefined) g draw (trimGuide.get.addAttributes("Color" -> new Color(0.95f, 0.15f, 0.80f, 1.00f))).transform (t)
  }
}