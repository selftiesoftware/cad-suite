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
import scala.Array._

class Offset extends Module {

  //a function to offset a line segment
  def offset(s : LineShape) : LineShape = {
    //TODO: this distance appears to be wrong.
    val mouseDistance = s.distanceTo(mousePosition)
    val lT = s.transform(TransformationMatrix(-s.p1,1)) //move the segment to 0,0
    val length : Int = ((s.p2 - s.p1).length).toInt //get the length of the segment
    val lS = lT.transform(TransformationMatrix.apply(Vector2D(0,0),1/length.toDouble))//scale it to the unit vector
    val lR = lS.transform(TransformationMatrix().rotate(90, Vector2D(0,0))) //rotate it 90 degrees
    val pt = if(lR.p1 == Vector2D(0,0)) lR.p2 else lR.p1  //get the point on the vector which is not on (0,0)
    val offsetDirection = -pt * mouseDistance //get the length of the offsetvector
    val offset = s.transform(TransformationMatrix(offsetDirection,1))//offset with the current distance
    offset
  }  
  var distancePoint : Option[Vector2D] = None
  var originals : Option[Selection] = None

  //a guide to get Point to draw the shape(s) dynamically
  val guide: PointGuide = PointGuide((v : Vector2D) => {
    val shape = Drawing.selection.head.shapes.head._2
    val vertices = shape.geometry.vertices
      //make a list of each line segment in the shape
      var l = List[LineShape]()

    def run = {
      for (i <- 0 to vertices.length-2) {
        l = l :+ offset(LineShape(vertices(i), vertices(i+1)))
      }
      //TODO: evaluate the segments in pairs - if their ends intersect, trim them. If not, extend them.
      l
    }
    println(run.toTraversable)
    run.toTraversable
    //Traversable(offset(LineShape(vertices(0), vertices(1))))

    //run the offset function

  },1)//1 : Input type = InputTwoValues

  //Select shapes
  val stateMap: StateMap = Map(

  'Start -> {
     case Message(p : Vector2D) :: tail => {
      println("A")
      distancePoint = Some(p)
    }

    case MouseUp(_, MouseButtonRight, _) :: tail => End
    case MouseUp(p, _, _) :: tail => {
      //goto 'Point to get the offset distance
    }
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

