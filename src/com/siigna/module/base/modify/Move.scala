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

object Move extends Module {

  var startPoint : Option[Vector2D] = None
  
  var shape : Traversable[ImmutableShape] = None
  
  var transformation : Option[TransformationMatrix] = None

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph(
    'Start      -> 'KeyDown -> 'End
  )
  
  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      if (Model.selection.isDefined) {
        startPoint = Some(Siigna.mousePosition)
        Goto('Move)
      } else {
        Goto('End)
      }
    }),
    'Move -> ((events : List[Event]) => {
      if (startPoint.isDefined) {
        events match {
          case (m : MouseDown) :: tail =>
          case MouseDrag(p, _, _) :: tail => {
            transformation = Some(TransformationMatrix(p - startPoint.get, 1))
            shape = Model.selection.get.apply(transformation.get)
          }
          case _ => Goto('End)
        }
      }
    }),
    'End   -> ((events : List[Event]) => {
      events match {
        case _ => {
          if (transformation.isDefined && Model.selection.isDefined) {
            Model.selection.get.transform(transformation.get)
            Model.deselect
          }
        }
      }
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (!shape.isEmpty) {
      shape.foreach(g draw _.transform(t))
    }
  }

}