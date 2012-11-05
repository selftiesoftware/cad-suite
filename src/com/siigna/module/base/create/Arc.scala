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

/* 2012 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._
import app.Siigna
import java.awt.Color

class Arc extends Module {

  var r = TransformationMatrix()

  var startPoint : Option[Vector2D] = None
  var endPoint : Option[Vector2D] = None

  def stateMap = Map(
    //StartCategory: Defines a start point for the arc and forwards to 'SetEndPoint
    'Start -> {
      case events => {
        events match {
          case MouseDown(_, MouseButtonRight, _) :: tail => 'End
          case End(p : Vector2D) :: tail => {
            if (startPoint.isEmpty) {
              println("Startpunkt sat")
              startPoint = Some(p)
              Start('Point, "com.siigna.module.base.create",
                //Guide(v => Traversable(CircleShape(p, math.abs((p - v).x))))
                Guide(v => Traversable(LineShape(p,v)))
              )
            } else if (endPoint.isEmpty) {
              println("Slutpunkt sat")
              println("Startpunkt: " + startPoint.get)
              println("Slutpunkt: " + p)
              println("forskel mellem start og slut: " + (((p - startPoint.get))) )
              println("Start plus forskel mellem start og slut: " + (startPoint.get + Vector2D(((p.x - startPoint.get.x)/2).toInt,((p.y - startPoint.get.y)/2).toInt) ))

              endPoint = Some(p)
              Start('Point, "com.siigna.module.base.create",
                //Guide(v => Traversable(CircleShape(p, math.abs((p - v).x))))
                Guide(v => Traversable(ArcShape(startPoint.get,v,endPoint.get)))
              )
            } else {

              //BUGFIX NEEDED: Crashes if tries to draw on same start or end point.

              val arc = ArcShape(startPoint.get,p,endPoint.get)
              
              def setAttribute[T : Manifest](name:String, shape:Shape) = {
                Siigna.get(name) match {
                  case s : Some[T] => shape.addAttribute(name, s.get)
                  case None => shape// Option isn't set. Do nothing
                }
              }

              Create(setAttribute[Color]("Color",
                setAttribute[Double]("LineWeight", arc)
              ))
              End
            }
          }
          //Hvis der er trykket escape eller højre museknap lukkes modulet, selvom der ikke er tegnet noget endnu
          case End :: tail => End
          case _ => {
            println("Ukendt kommando i Arc-modulet")
            Start('Point, "com.siigna.module.base.create")
          }
        }
      }
    }
  )

}



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

/*package com.siigna.module.base.create

/* 2012 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._

class Arc extends Module {

  //a flag used to determinie when to stop drawing a circle as a dynamic shape
  private var inSetArc = false

  private var secondArcPoint : Option[Vector2D] = None

  // The points of the polyline
  private var points   = List[Vector2D]()

  val eventHandler = EventHandler(stateMap, stateMachine)

  private var secondPointSet = false

  def stateMap = DirectedGraph(
    'StartCategory        ->   'Message   ->    'SetRadius,
    'SetRadius    ->   'KeyEscape ->    'End,
    'SetArc       ->   'KeyEscape ->    'End
  )


  def stateMachine = Map(
  'StartCategory -> ((events : List[Event]) => {
    Siigna.display("Set the startpoint, then radius, and endpoint")
    //Log.level += Log.DEBUG + Log.SUCCESS
    events match {
      case MouseDown(_, MouseButtonRight, _) :: tail => {
        Goto('End)
      }
      case _ => Module('Point)
    }
  }),
  'SetRadius -> ((events : List[Event]) => {
    //before enough information is gathered to send an arcShape as a PointGuide, a circle is sent.
    val getCircleGuide : Vector2D => CircleShape = (v : Vector2D) => {
       CircleShape(v, points.head)
     }

    events match {
      // Exit mechanisms
      case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

      case Message(p : Vector2D) :: tail => {

        // Send a circle as a PointGuide to Point if there is enough points set
        if (points.length < 1) {
          //proceed to set the Arc segment
          points = points :+ p
          Controller ! Message(PointGuide(getCircleGuide))
          Module('Point)
        }
        else if (points.length == 1) {
          //proceed to set the Arc segment
          points = points :+ p
          Goto('SetArc)
        }
      }
      // Match on everything else
      case _ =>
    }
  }),
  'SetArc -> ((events : List[Event]) => {
    inSetArc = true
    val arcGuide : Vector2D => ArcShape = (v : Vector2D) => {
       val radius = (points(0)-points(1)).length
       val a1 = (points(0)-points(1)).angle
       val a2 = (points(0)-v).angle
       ArcShape(points(1), radius, a1, a2-360)
     }
    events match {
      case Message(p : Vector2D) :: tail => {
      }
      case MouseDown(_ ,MouseButtonRight, _) :: tail => Goto('End)
      case MouseUp(p , _, _) :: tail => {
        if(secondPointSet == true){
          secondArcPoint = Some(p)
          Goto('End)
        }
        secondPointSet = true
      }

      case _ => {
        Controller ! Message(PointGuide(arcGuide))
        Module('Point)
      }


    }
  }),
  'End -> ((events : List[Event]) => {

    CreateCategory(ArcShape(points(1),(points(0)-points(1)).length,(points(0)-points(1)).angle,(points(0)-secondArcPoint.get).angle-360))

    //clear the vars
    inSetArc = false
    points = List()
    secondPointSet = false
  }))
  //draw a guiding circle when relevant.
  override def paint(g : Graphics, t : TransformationMatrix) {
    if(inSetArc == false && points.length == 2)
      g draw CircleShape(points(1), points(0)).transform(t)
  }
}*/