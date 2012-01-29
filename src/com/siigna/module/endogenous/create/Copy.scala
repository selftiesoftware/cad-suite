package com.siigna.module.endogenous.create

/* 2010 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._

object Copy extends Module {

  var basePoint : Option[Vector2D] = None
  var endPoint : Option[Vector2D] = None
  var shapes : Iterable[DynamicShape] = Iterable()

  def delta : Option[Vector2D] = if (basePoint.isDefined && endPoint.isDefined) Some(endPoint.get - basePoint.get) else None

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph('Start -> 'MouseMove -> 'Move,
                                        'Start -> 'MouseUp   -> 'End,
                                        'Move  -> 'MouseUp   -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      if (!Model.isSelected) ForwardTo('Select) // Prompt for selection
      else {
        shapes = Model.selected
        events match {
          case MouseMove(point, _, _) :: tail => basePoint = Some(point)
          case MouseDown(point, _, _) :: tail => basePoint = Some(point)
          case MouseDrag(point, _, _) :: tail => basePoint = Some(point)
          case _ => basePoint = Some(Siigna.mousePosition)
        }
      }
    }),
    'Move -> ((events : List[Event]) => {
      events match {
        case MouseMove(p, _, _) :: tail => endPoint = Some(p)
        case MouseDrag(p, _, _) :: tail => endPoint = Some(p)
        case KeyUp(Key.control, _) :: tail => endPoint = Some(Siigna.mousePosition); Goto('End)
        case _ =>
      }
    }),
    'End   -> ((events : List[Event]) => {
      if (delta.isDefined && events.head != KeyDown(Key.Esc, ModifierKeys(false, false, false))) {
        Create(shapes.map(_.shape.transform(TransformationMatrix(delta.get, 1))))
      }
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (delta.isDefined) {
      shapes. foreach( g draw _.transform(t.translate(delta.get)))
    }
  }

}
