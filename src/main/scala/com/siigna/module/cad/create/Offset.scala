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

package com.siigna.module.cad.create

import com.siigna._
import app.model.shape.PolylineShape.{PolylineShapeOpen, PolylineShapeClosed}
import com.siigna.app.model.shape.RectangleShape

import module.Tooltip

class Offset extends Module {
  private var attr = Attributes()
  private var done = false
  private var isClosed = false
  private val shape: Option[Shape] = if (Drawing.selection.isDefined) Some(Drawing.selection.shapes.head._2) else None
  private val isRect = if(shape.isDefined) shape.get.isInstanceOf[RectangleShape] else false


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
  //a function to generate a Shape from the result of the offsetLines function
  def generateOffsetLine(s: Any) : Option[Shape] = {

    shape.get match {
      case pso: PolylineShapeOpen => {
        offsetLinesForLineAndPolylineShapes (s)
      }
      case psc: PolylineShapeClosed => {
        offsetLinesForLineAndPolylineShapes (s)
      }
      case ls: LineShape => {
        offsetLinesForLineAndPolylineShapes (s)
      }
      case rs: RectangleShape => {
        offsetLinesForLineAndPolylineShapes (s)
      }

      case cs: CircleShape => {
        s match {
          case v: Vector2D => {
            if (cs.center.distanceTo(mousePosition.transform(View.deviceTransformation)) == cs.radius || cs.center.distanceTo(mousePosition.transform(View.deviceTransformation)) == 0) None
            else Some(CircleShape(cs.center,v))
          }
          case d: Double => {
            if (cs.center.distanceTo(mousePosition.transform(View.deviceTransformation)) > cs.radius) Some(CircleShape(cs.center,cs.radius + d))
            else if (cs.center.distanceTo(mousePosition.transform(View.deviceTransformation)) < cs.radius && d < cs.radius) Some(CircleShape(cs.center,cs.radius - d))
            else {
              Siigna display ("Offset distance inside circle must be less than circle radius")
              None
            }
          }
          case x => {
            println("Ununderstandable circle shape: " + x)
            None
          }
        }
      }
      case as: ArcShape => {
        s match {
          case v: Vector2D => {
            if (as.center.distanceTo(mousePosition.transform(View.deviceTransformation)) == as.radius || as.center.distanceTo(mousePosition.transform(View.deviceTransformation)) == 0) None
            else Some(ArcShape(as.center,as.center.distanceTo(mousePosition.transform(View.deviceTransformation)),as.startAngle,as.angle))
          }
          case d: Double => {
            if (as.center.distanceTo(mousePosition.transform(View.deviceTransformation)) > as.radius) Some(ArcShape(as.center,as.radius + d,as.startAngle,as.angle))
            else if (as.center.distanceTo(mousePosition.transform(View.deviceTransformation)) < as.radius && d < as.radius) Some(ArcShape(as.center,as.radius - d,as.startAngle,as.angle))
            else {
              Siigna display ("Offset distance inside arc must be less than arc radius")
              None
            }
          }
          case x => {
            println("Ununderstandable arc shape: " + x)
            None
          }
        }
      }

      case x => {
        println("Shape not caught: " + x)
        None
      }
    }
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
  //get the start and end points for the list of points that make up an offset, closed polyline 
  def getClosedOffsetPoint (l : List[LineShape]) : Vector2D = {
    val segment1 = Line2D(l(0).p1,l(0).p2)
    val segment2 = Line2D(l.reverse(0).p1,l.reverse(0).p2)
    val p = segment1.intersections(segment2)
    p.head
  }

  def offsetLinesForLineAndPolylineShapes(s: Any): Option[Shape] = {
    //check if the shape to offset is closed (closed Polyline of ComplexRectangle)
    if(shape.get.geometry.vertices.head == shape.get.geometry.vertices.last ||isRect) isClosed = true
    var newLines: Option[List[LineShape]] = None

    s match {
      case v: Vector2D => newLines = Some(offsetLines(shape.get, v))  //offset the lines by the returned point
      case d: Double => newLines = Some(offsetLines(shape.get, d))    //offset the lines by the returned point
      case _ =>
    }

    var knots = List[Vector2D]()
    knots = knots :+ newLines.get.head.p1 //add the first point to the list

    getKnots(newLines.get).foreach(s => knots = knots :+ s) //add the intersections to the knots list

    knots = knots :+ newLines.get.reverse.head.p2 //add the last point to the list

    //if the polyline is closed, calculate the offset of the closing point and add it to the start and end of the list
    if(isClosed == true) {
      val closedPt = getClosedOffsetPoint(newLines.get)
      knots = knots.tail.take(knots.size - 2) //remove the first and last element
      knots = knots :+ closedPt //prepend the closed offset point to the list
      knots = knots.reverse :+ closedPt //append the closed offset point to the list
      knots = knots.reverse
    }

    shape.get match {
      case s: PolylineShape => Some(PolylineShape(knots))
      case s: LineShape => Some(LineShape(knots(0), knots(1)))
      case s: RectangleShape => {
        Some(RectangleShape(knots(0), knots(2)))
      }
      case x => {
        println("Shape type not recognised in Offset: " + x )
        None
      }
    }
  }

  //calculate on which side of a given line segment the offset should be.
  def offsetSide (s: LineShape, m : Vector2D) : Boolean = {
    val angleRaw = ((s.p2-s.p1).angle * -1) + 450
    val angle = if(angleRaw > 360) angleRaw - 360 else angleRaw
    val linePt = s.geometry.closestPoint(m)
    val lx = linePt.x
    val mx = m.x
    val ly = linePt.y
    val my = m.y

    //the mouse point projected perpendicularily on the the line is used to evaluate offset side.
    //if the mouse y is north of the shape, offset should extend upwards. This eval does not work on vertical lines
    //in these cases the x values are evaluated.

    if(angle > 0 && angle <= 180) {
      if(my > ly) false
      else if (my == ly && mx > lx) false //evaluate x values instead when lines are vertical
      else true
    }
    //if the mouse is south of the shape, offset should extend downwards
    else {
      if(my < ly) false
      else if (my == ly && mx < lx) false //evaluate x values instead when lines are vertical
      else true
    }
  }

  //calculates the distance to offset shapes by, then calls the offset function to returns offset shapes as a list
  def offsetLines(s : Shape, m : Vector2D) : List[LineShape] = {
    var l = List[LineShape]()
    var v = s.geometry.vertices
    //add a vertex if the shape is a ComplexRect
    if(isRect) v = v :+ v(0)
    //iterate through the shapes to find the shape closest to the mouse
    def calcNearest : Double = {
      var nearestDist : Double = LineShape(v(0), v(1)).distanceTo(m)
      var closestSegment : Option[LineShape] = Some(LineShape(v(0), v(1)))
      for (i <- 0 to v.length -2) {
        val currentSegment = LineShape(v(i), v(i+1))
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

  def offsetLines(s : Shape, d : Double) = {
    var l = List[LineShape]()
    var v = s.geometry.vertices
    val m = mousePosition.transform(View.deviceTransformation)
    //add a vertex if the shape is a ComplexRect
    if(isRect) v = v :+ v(0)
    //iterate through the shapes to find the shape closest to the mouse
    def calcNearest : Double = {
      var nearestDist : Double = LineShape(v(0), v(1)).distanceTo(m)
      var closestSegment : Option[LineShape] = Some(LineShape(v(0), v(1)))
      for (i <- 0 to v.length -2) {
        val currentSegment = LineShape(v(i), v(i+1))
        if (currentSegment.distanceTo(m) < nearestDist) {
          closestSegment = Some(currentSegment)
          nearestDist = currentSegment.distanceTo(m)
        }
      }
      //check on which side of the original the offset should take place
      val n = if(closestSegment.isDefined && offsetSide(closestSegment.get, m) == true) d
      else  - d
      n
    }

    val distance : Double = calcNearest
    //run the offset function to offset shapes with the distance set in calcNearest
    for (i <- 0 to v.length-2) l = l :+ calcOffset(LineShape(v(i), v(i+1)),distance)
    l//return the list
  }

  //guides to get Point to draw the shape(s) dynamically
  val doubleGuide: DoubleGuide = DoubleGuide((s : Double) => {
    if (generateOffsetLine(s).isDefined) Traversable(generateOffsetLine(s).get.addAttributes(attr))//run a function to generate the offset shape dynamically
    else Drawing.selection.shapes.values
  })
  val vector2DGuide: Vector2DGuide = Vector2DGuide((v : Vector2D) => {
    if (generateOffsetLine(v).isDefined)Traversable(generateOffsetLine(v).get.addAttributes(attr))//run a function to generate the offset shape dynamically
    else Drawing.selection.shapes.values
  })

  //Select shapes
  val stateMap: StateMap = Map(

  'Start -> {
    case End(p : Vector2D) :: tail => {
      done = true
      if (generateOffsetLine(p).isDefined) Create(generateOffsetLine(p).get.addAttributes(attr))//create a polylineShape from the offset knots:
      End
    }

    case End(d : Double) :: tail => {
      done = true
      if (generateOffsetLine(d).isDefined) Create(generateOffsetLine(d).get.addAttributes(attr))//create a polylineShape from the offset knots:
      End
    }

    case MouseUp(_, MouseButtonRight, _) :: tail => End

    //exit strategy
    case KeyDown(Key.Esc, _) :: tail => End
    case MouseDown(p, MouseButtonRight, _) :: tail => End
    case End(KeyDown(Key.Esc, _)) :: tail => End
    case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

    case _ => {
      if (Drawing.selection.isDefined == false && done == false) {
        Tooltip.updateTooltip("Offset tool active")
        Siigna display "select an object to offset"
        Tooltip.blockUpdate(3500)
        Start('cad, "Selection")
      }
      else if (Drawing.selection.size == 1 ){
        attr = Drawing.selection.shapes.head._2.attributes
        Tooltip.updateTooltip("Offset tool active")
        Siigna display "click to set the offset distance, or type offset distance"
        Tooltip.blockUpdate(3500)
        val inputRequest = InputRequest(9,None,vector2DGuide, doubleGuide)
        Start('cad,"create.Input", inputRequest)
      } else if (done) End
      else {
        Siigna display "please select one shape to offset"
        Tooltip.blockUpdate(3500)
        Drawing.deselect()
        End
      }
    }
  })
}

