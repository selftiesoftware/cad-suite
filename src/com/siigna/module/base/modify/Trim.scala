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

/*
package com.siigna.module.base.modify

/* 2010 (C) Copyright by Siigna, all rights reserved. */


import com.siigna._

class Trim extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  var selection = List[Vector2D]()
  var shapes : List[Shape] = List()

  var trimGuide : Option[Shape] = None
  var trimShapes : Iterable[Shape] = List()

  var selectionBoxStart : Option[Vector2D] = None
  var selectionBoxEnd : Option[Vector2D] = None

      /*
        Function:   if lines are already selected: go to trim mode.
                    if lines are NOT selected: Use the select module to choose lines.

                    Trim:
                    If mouseclick: Remove the selected linesegment until next (selected) intersection.
                    If mousedrag (selection): Remove all line segments hit by the selection-boxen until next (selected) intersection.

                    End:
                    Esc or right click.
      */

  lazy val stateMap  = DirectedGraph  ('StartCategory            -> 'MouseMove -> 'StartCategory,
                                       'StartCategory            -> 'MouseDown -> 'SelectLines,
                                       'StartCategory            -> 'KeyEscape -> 'End,
                                       'SelectLines      -> 'KeyEscape -> 'End,
                                       'SelectionBox     -> 'KeyEscape -> 'End,
                                       'StartSelection   -> 'KeyEscape -> 'End)

 lazy val stateMachine = Map(
  //check if a selection of objects has already been made:
  'StartCategory ->  ((events : List[Event]) => {
    events match {
      case MouseMove(point, _, _) :: tail => {
        if (Drawing.selected.size == 1) {
          trimGuide = Some(Drawing.selected.head.shape)
          Goto('StartSelection)
        } else {
          Siigna.display("Select an object to trim objects by")
          Module('Select)
        }
      }
      case _ =>
     }
    None
   }),

   'SelectLines ->  ((events : List[Event]) => {
     events match {
       case MouseDown (p,_,_) :: tail => {
         val closestShape = Drawing(p)
         if (closestShape.isDefined && closestShape.get.distanceTo(p) < 5 && closestShape.get.isInstanceOf[Shape]) {
           Drawing.select(Drawing.findId(_ == closestShape.get.asInstanceOf[Shape]))
           if (Drawing.selected.size == 1) {
             trimGuide = Some(Drawing.selected.head.shape)
             Goto('StartSelection)
           }
         }
       }
       case _ =>
     }
     None
   }),

  //StartSelection: StartCategory selecting line(s).
  'StartSelection -> ((events : List[Event]) => {
    Siigna.display("click or drag mouse to trim lines")
      events match {
        case MouseUp(p2, _, _) :: _ :: MouseDown(p1, _, _) :: tail => {
          if ((p2 - p1).length <= 2) {
            val closestShape = Drawing(p2)
            if (closestShape.isDefined && closestShape.get.distanceTo(p2) <= 10 && closestShape.isInstanceOf[Shape]) {
              trimShapes ++:= Iterable(closestShape.get.asInstanceOf[Shape])
            }
          }
        }
        case MouseDrag(p2, _, _) :: MouseDown(p1, _, _) :: tail => {
          if ((p2 - p1).length > 2) {
            selectionBoxStart = Some(p1)
            selectionBoxEnd = Some(p2)
            Goto('SelectionBox)
          }
        }
        case _ =>
       }
      None
    }),

    'SelectionBox -> ((events : List[Event]) => {
      events match {
        case MouseDrag(p, _, _) :: tail => {
          selectionBoxEnd = Some(p)
        }
        case MouseUp(p, _, _) :: tail => {
          selectionBoxEnd = Some(p)
          val box = Rectangle2D(selectionBoxStart.get, selectionBoxEnd.get)
          trimShapes = Drawing.queryForShapes(box).filter(s => s.geometry.intersects(box))
          Goto('End)
        }
        case _ =>
      }
      None
    }),

    'End -> ((events : List[Event]) => {
      try {
        val lineGuide  = trimGuide.get.asInstanceOf[PolylineShape]
        val lineShapes = trimShapes.filter(_.isInstanceOf[PolylineShape])

        //lineShapes.foreach(polyline => {
        //  val intersections = polyline.intersects(lineGuide)
        //})
      } catch {
        case e => Log.error("Trimming only works with polylines for now.")
      }
      None
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (selectionBoxStart.isDefined && selectionBoxEnd.isDefined) {
      val box = Rectangle2D(selectionBoxStart.get, selectionBoxEnd.get)
      //TODO: activate drawing once polyline is created
      //g draw PolylineShape.fromRectangle(box).attributes_+=("Color" -> "#66CC66".color).transform(t)
    }
  }

}


*/
