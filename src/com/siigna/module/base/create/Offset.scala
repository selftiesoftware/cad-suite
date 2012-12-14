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
import java.lang.Double.POSITIVE_INFINITY

class Offset extends Module {

  //a function to offset a line segment
  def offset(s : LineShape, dist : Double) : LineShape = {
    val lT = s.transform(TransformationMatrix(-s.p1,1)) //move the segment to 0,0
    val length : Int = ((s.p2 - s.p1).length).toInt //get the length of the segment
    val lS = lT.transform(TransformationMatrix.apply(Vector2D(0,0),1/length.toDouble))//scale it to the unit vector
    val lR = lS.transform(TransformationMatrix().rotate(90, Vector2D(0,0))) //rotate it 90 degrees
    val pt = if(lR.p1 == Vector2D(0,0)) lR.p2 else lR.p1  //get the point on the vector which is not on (0,0)
    val offsetDirection = -pt * dist //get the length of the offsetvector
    val offset = s.transform(TransformationMatrix(offsetDirection,1))//offset with the current distance
    offset
  }  
  var distancePoint : Option[Vector2D] = None
  var originals : Option[Selection] = None
  
  //a guide to get Point to draw the shape(s) dynamically
  val guide: PointGuide = PointGuide((v : Vector2D) => {
    val shape = Drawing.selection.head.shapes.head._2
    //TODO: get the offset distance by using the segment which has the shortest mouseDistance.
    val vertices = shape.geometry.vertices
    //make a list of each line segment in the shape
    var l = List[LineShape]()
    val m = mousePosition.transform(View.deviceTransformation)

    def offsetLines = {
      var nearestShape = LineShape(vertices(0), vertices(1))

      //iterate through the shapes to find the shape closest to the mouse
      def calcNearest = for (i <- 0 to vertices.length-2) {
        val s = LineShape(vertices(i), vertices(i+1))
        if (s.distanceTo(m) < nearestShape.distanceTo(m)) nearestShape = s
      }
      calcNearest
      val distance = nearestShape.distanceTo(mousePosition.transform(View.deviceTransformation))

      //offset the shapes with the distance set in calcNearest
      for (i <- 0 to vertices.length-2) {
        
        l = l :+ offset(LineShape(vertices(i), vertices(i+1)),distance)
      }
      l
    }
    //extend the offset lines so adjecent lines always intersect (unless they are parallel)

    def extend(s : List[LineShape]) = {
      var list = List[LineShape]()
      s.foreach(t => list = list :+ t.transform(TransformationMatrix().scale(2,t.geometry.center)))
      list
    }

    val infinityOffsets = extend(offsetLines)
    var knots = List[Vector2D]()
    //add the first point to the list
    knots = knots :+ l.head.p1
    //println("knots before: "+knots)
    val getKnots = {
      var k =  List[Vector2D]()
      for (i <- 0 to infinityOffsets.length -2) {
        val s1 = infinityOffsets(i)
        val s2 = infinityOffsets(i+1)
        val l1 = Line2D(s1.p1,s1.p2)
        val l2 = Line2D(s2.p1,s2.p2)
        val int = l1.intersections(l2).head
        k = k :+ int
      }
      k
    }
    def result = getKnots.foreach(s => knots = knots :+ s) //add the intersections to the konts list
    result
    //add the last
    knots = knots :+ l.reverse.head.p2

    //create a polylineShape from the offset knots:
    Array(PolylineShape(knots))

  },1)//1 : Input type = InputTwoValues

  //Select shapes
  val stateMap: StateMap = Map(

  'Start -> {
     case Message(p : Vector2D) :: tail => {
      distancePoint = Some(p)
    }

    case MouseUp(_, MouseButtonRight, _) :: tail => End
    case MouseUp(p, _, _) :: tail => {
      //goto 'Point to get the offset distance
    }

    //exit strategy
    case KeyDown(Key.Esc, _) :: tail => End
    case MouseDown(p, MouseButtonRight, _) :: tail => End

    case _ => {
      if (!Drawing.selection.isDefined) {
        Siigna display "select an object to offset first"
        End
      }
      else if (Drawing.selection.isDefined && Drawing.selection.get.size == 1 && !distancePoint.isDefined){
        Siigna display "click to set the offset distance"
        Start('Input,"com.siigna.module.base.create", guide)
      }
    }
  })

    //do the offset calculation here
    //'End -> ((events : List[Event]) => {
      //get the point on the line that is perpendicular to the distance point
      //if(distancePoint.isDefined) {
      //  val transformation : TransformationMatrix = TransformationMatrix(distancePoint.get, 1)
      //  Drawing.selection.get.apply(transformation)
      //}

      //println("END")
      //println("offset here with distance: "+distancePoint)
      //Drawing.deselect()
  //}))


}

