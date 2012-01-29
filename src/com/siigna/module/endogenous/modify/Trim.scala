package com.siigna.module.endogenous.modify

/* 2010 (C) Copyright by Siigna, all rights reserved. */


import com.siigna._

object Trim extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  var selection = List[Vector2D]()
  var shapes : List[Shape] = List()

  var trimGuide : Option[ImmutableShape] = None
  var trimShapes : Iterable[ImmutableShape] = List()

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

  lazy val stateMap  = DirectedGraph  ('Start            -> 'MouseMove -> 'Start,
                                       'Start            -> 'MouseDown -> 'SelectLines,
                                       'Start            -> 'KeyEscape -> 'End,
                                       'SelectLines      -> 'KeyEscape -> 'End,
                                       'SelectionBox     -> 'KeyEscape -> 'End,
                                       'StartSelection   -> 'KeyEscape -> 'End)

 lazy val stateMachine = Map(
  //check if a selection of objects has already been made:
  'Start ->  ((events : List[Event]) => {
    events match {
      case MouseMove(point, _, _) :: tail => {
        if (Model.selected.size == 1) {
          trimGuide = Some(Model.selected.head.shape)
          Goto('StartSelection)
        } else {
          Siigna.display("Select an object to trim objects by")
          ForwardTo('Select)
        }
      }
      case _ =>
     }
    None
   }),

   'SelectLines ->  ((events : List[Event]) => {
     events match {
       case MouseDown (p,_,_) :: tail => {
         val closestShape = Model(p)
         if (closestShape.isDefined && closestShape.get.distanceTo(p) < 5 && closestShape.get.isInstanceOf[ImmutableShape]) {
           Model.select(Model.findId(_ == closestShape.get.asInstanceOf[ImmutableShape]))
           if (Model.selected.size == 1) {
             trimGuide = Some(Model.selected.head.shape)
             Goto('StartSelection)
           }
         }
       }
       case _ =>
     }
     None
   }),

  //StartSelection: Start selecting line(s).
  'StartSelection -> ((events : List[Event]) => {
    Siigna.display("click or drag mouse to trim lines")
      events match {
        case MouseUp(p2, _, _) :: _ :: MouseDown(p1, _, _) :: tail => {
          if ((p2 - p1).length <= 2) {
            val closestShape = Model(p2)
            if (closestShape.isDefined && closestShape.get.distanceTo(p2) <= 10 && closestShape.isInstanceOf[ImmutableShape]) {
              trimShapes ++:= Iterable(closestShape.get.asInstanceOf[ImmutableShape])
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
          trimShapes = Model.queryForShapes(box).filter(s => s.geometry.intersects(box))
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
