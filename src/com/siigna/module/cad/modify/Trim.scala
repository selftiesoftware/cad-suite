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

import com.siigna._
import util.geom.Geometry2D
import java.awt.Color

class Trim extends Module {

  private var attr = Attributes()
  private var done = false
  private var nearestShape : Option[Shape] = None
  private var trimGuide : Option[Shape] = None
  private var trimGuide2 : Option[Shape] = None
  private var trimID : Option[Int] = None
  private var trimID2 : Option[Int] = None

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
  def getIntersectSegmentNumber(g : Geometry2D, t : Geometry2D) : (List[Int],List[Int]) = {
    val gV = g.vertices
    val tV = t.vertices
    var gS = List[Int]()
    var tS = List[Int]()
    if(gV.length >= 2 && tV.length >= 2) {
      for (i <- 0 to tV.length - 2) {
        for(j <- 0 to gV.length - 2) {
          if(IntersectEval(tV(i),tV(i+1),gV(j),gV(j+1)) == true){

            gS = gS :+ j
            tS = tS :+ i
          }
        }
      }
    }
    (gS, tS)
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
  def trim(guide : Geometry2D, trimLine : Geometry2D, p : Vector2D) : (List[Vector2D], List[Vector2D]) = {
    var trimV1 = List[Vector2D]()
    var trimV2 = List[Vector2D]()
    val g : Seq[Vector2D] = guide.vertices
    val t : Seq[Vector2D] = trimLine.vertices
    val intSegment = getIntersectSegmentNumber(guide,trimLine) //find segment numbers for intersections
    val intSegment1 : List[Int] = getIntersectSegmentNumber(guide,trimLine)._1 //find segment numbers for intersections
    val intSegment2 : List[Int] = getIntersectSegmentNumber(guide,trimLine)._2 //find segment numbers for intersections

    //store return values for the first intersection;
    val guideSegmentNr1 = intSegment1(0)
    val guideSegmentNr2 = intSegment1(1)
    val trimSegmentNr1 = intSegment2(0)
    val trimSegmentNr2 = intSegment2(1)

    println("guideSegmentNr1: "+guideSegmentNr1)
    println("guideSegmentNr2: "+guideSegmentNr2)
    println("trimSegmentNr1: "+trimSegmentNr1)
    println("trimSegmentNr2: "+trimSegmentNr2)

    val guideSegment1 = Line2D(g(guideSegmentNr1),g(guideSegmentNr1+1))
    val guideSegment2 = Line2D(g(guideSegmentNr2),g(guideSegmentNr2+1))
    val trimSegment1 = Line2D(t(trimSegmentNr1),t(trimSegmentNr1+1))
    val trimSegment2 = Line2D(t(trimSegmentNr2),t(trimSegmentNr2+1))
    val int1 = guideSegment1.intersections(trimSegment1)   //do the intersection!
    val int2 = guideSegment2.intersections(trimSegment2)   //do the intersection!


    //one or first intersection - get the trimline
    if (!int1.isEmpty) {
      val getSegment = (getSubSegment(t(trimSegmentNr1),t(trimSegmentNr1+1),int1.head,p))
      val twoLists = splitPolyline(t.toList,int1.head)//the trimline split into two lists

      if(getSegment == 1) { //if the trim point is set before the trimline, keep the first segments.
        trimV1 = (twoLists._1 :+ t.last).reverse :+ int1.head
      }
      else if(getSegment == 2) { //if the trim point is set after the trimline, keep the last segments.
        trimV1 = twoLists._2 :+ int1.head
      } else trimV1 = trimLine.vertices.toList
    }
    //if two intersections between the trimline and guideline are found, get the second trimline:
    if (!int2.isEmpty) {
      val getSegment = (getSubSegment(t(trimSegmentNr2),t(trimSegmentNr2+1),int2.last,p))
      val twoLists = splitPolyline(t.toList,int2.head)//the trimline split into two lists

      if(getSegment == 1) { //if the trim point is set before the trimline, keep the first segments.
        trimV2 = (twoLists._1 :+ t.last).reverse :+ int2.head
      }
      else if(getSegment == 2) { //if the trim point is set after the trimline, keep the last segments.
        trimV2 = twoLists._2 :+ int2.head
      } else trimV2 = trimLine.vertices.toList
    }
    //else trimV = t.toList //if not intersection is found, return the original trimLine
    (trimV1, trimV2) //return the trimmed line
  }

  val stateMap: StateMap = Map(
    'Start -> {
      //exit strategy
      case KeyDown(Key.escape, _) :: tail => {
        Track.trackEnabled = true
        Drawing.deselect()
        End
      }
      case MouseDown(p, MouseButtonRight, _) :: tail => {
        Track.trackEnabled = true
        Drawing.deselect()
        End
      }

      //if the input module returns an ESC or right mouse down, end the module. -And reenable Track.
      case End(KeyDown(Key.escape,modifier)) :: tail => {
        Track.trackEnabled = true
        Drawing.deselect()
        End
      }
      case End(MouseDown(_ , MouseButtonRight, _)) :: tail => {
        Track.trackEnabled = true
        Drawing.deselect()
        End
      }

      //if selection returns a point, evaluate if there are any shapes to trim at that point:
      case End(v : Vector2D) :: tail => {
        if(trimGuide.isDefined && !trimGuide2.isDefined) {
          val m = mousePosition.transform(View.deviceTransformation)
          //find the shape closest to the mouse:
          if (Drawing(m).size > 0) {
            val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
            nearestShape = if (nearest._2.distanceTo(m) < Siigna.selectionDistance) Some(nearest._2) else None
            attr = nearestShape.get.attributes

            val trimShape = nearestShape.get.geometry
            val guideShape = trimGuide.get.geometry

            //do the trimming:
            Delete(nearest._1)
            val lists = trim(guideShape, trimShape, v)

            //create the first trimmed line
            Create(PolylineShape(lists._1).addAttributes(attr))

            //if two trimline are to be made (trimV2 is defined), create the second line
            if(!lists._2.isEmpty) {
              Create(PolylineShape(lists._2).addAttributes(attr))
            }

            nearestShape = None //reset var
            done = true
            Start('cad, "create.Input", 1) //look for more trim points
          } else {

            Start('cad, "create.Input", 1)
          } //if the point is not set next to a shape, goto selection and try again
        }
        //if two trim guides have been selected, trim the shape with both.
        else if(trimGuide.isDefined && trimGuide2.isDefined) {
          val m = mousePosition.transform(View.deviceTransformation)
          //find the shape closest to the mouse:
          if (Drawing(m).size > 0) {
            val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
            nearestShape = if (nearest._2.distanceTo(m) < Siigna.selectionDistance) Some(nearest._2) else None
            attr = nearestShape.get.attributes

            val trimShape = nearestShape.get.geometry

            val guideShape1 = trimGuide.get.geometry
            val guideShape2 = trimGuide2.get.geometry

            //do the trimming:
            val lists1 = trim(guideShape1, trimShape, v)
            val lists2 = trim(guideShape2, trimShape, v)
            Delete(nearest._1)

            //create the first trimmed line
            Create(PolylineShape(lists1._1).addAttributes(attr))

            //if two trimline are to be made (trimV2 is defined), create the second line
            if(!lists1._2.isEmpty) {
              Create(PolylineShape(lists1._2).addAttributes(attr))
            }

            //create the second trimmed line
            Create(PolylineShape(lists2._1).addAttributes(attr))

            //if two trimline are to be made (trimV2 is defined), create the second line
            if(!lists2._2.isEmpty) {
              Create(PolylineShape(lists2._2).addAttributes(attr))
            }


            done = true
            nearestShape = None //reset var
            Start('cad, "create.Input", 1) //look for more trim points
          } else {
            Start('cad, "create.Input", 1)
          } //if the point is not set next to a shape, goto selection and try again
        }
        else {
          Track.trackEnabled = true
          End
        }
      }
      case _ => {
        Track.trackEnabled = false
        if (Drawing.selection.size == 1 && done == false) {
          trimGuide = Some(Drawing.selection.shapes.head._2)
          trimID = Some(Drawing.selection.shapes.head._1)
          Siigna.display("Select shapes to trim")
          Start('cad, "create.Input", 1)
        }
        else if(Drawing.selection.size == 2  && done == false) {
          trimGuide = Some(Drawing.selection.shapes.head._2)
          trimID = Some(Drawing.selection.shapes.head._1)
          trimGuide2 = Some(Drawing.selection.shapes.last._2)
          trimID2 = Some(Drawing.selection.shapes.last._1)
          Siigna.display("Select shapes to trim")
          Start('cad, "create.Input", 1)
        }
        else{  //Siigna.display("Select an object to trim objects by")
          Track.trackEnabled = true
          End
        }
      }
    })
  override def paint(g : Graphics, t : TransformationMatrix) {
    if(trimGuide.isDefined && !trimGuide2.isDefined) {
      g draw (trimGuide.get.addAttributes("StrokeWidth" -> 1.5, "Color" -> new Color(0.95f, 0.15f, 0.80f, 1.00f))).transform (t)
    }
    else if(trimGuide.isDefined && trimGuide2.isDefined) {
      g draw (trimGuide.get.addAttributes("StrokeWidth" -> 1.5, "Color" -> new Color(0.95f, 0.15f, 0.80f, 1.00f))).transform (t)
      g draw (trimGuide2.get.addAttributes("StrokeWidth" -> 1.5, "Color" -> new Color(0.95f, 0.15f, 0.80f, 1.00f))).transform (t)

    }
  }
}