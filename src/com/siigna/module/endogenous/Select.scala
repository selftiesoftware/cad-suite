/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

class Select extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  var box = Rectangle(Vector(0, 0), Vector(0, 0))

  var closeToObjectOnStart = false

  //TODO: Temporary! Create a dynamic model...!
  var selectedShape : Option[Shape] = None

  var boxedShapes : Seq[Shape] = Seq()

  def isEnclosed : Boolean = (box.p1.x <= box.p2.x)

  lazy val stateMap     = DirectedGraph('Start -> 'MouseMove -> 'Box,
                                        'Start -> 'MouseDrag -> 'Box,
                                        'Start -> 'MouseUp   -> 'End,
                                        'Start -> 'KeyEscape -> 'End,
                                        'Box   -> 'KeyEscape -> 'End,
                                        'Box   -> 'MouseUp   -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case (_ : MouseUp) :: (_ : MouseDown) :: tail => Goto('End)
        case MouseDown(point, _, _) :: tail => {
          selectedShape = Model(point)
          if (selectedShape.isDefined && selectedShape.get.distanceTo(point) < 10)
            closeToObjectOnStart = true
          else
            box = Rectangle(point, point)
        }
        case _ =>
      }
      None
    }),
    'Box   -> ((events : List[Event]) => {
      events match {
        case MouseMove(point, _, _) :: tail => box = Rectangle(box.p1, point)
        case MouseDrag(point, _, _) :: tail => {
          if (closeToObjectOnStart && selectedShape.isDefined) {
            Goto('End)
            ForwardTo('Move)
          } else
            box = Rectangle(box.p1, point)
        }
        case _ =>
      }
      boxedShapes = Model(box.toMBR)
      None
    }),
    'End   -> ((events : List[Event]) => {
      events match {
        //case (_ : MouseUp) :: (_ : MouseDown) :: MouseUp(p, _, ModifierKeys(_, true, _)) :: tail => {
        //  val parent = DOM.lookup(DOM.getShapeFrom(p))
        //  if (parent.isDefined) Some(UpdateShape(parent.get, parent.get.deselect))
        //  else None
        //}
        //case (_ : MouseUp) :: (_ : MouseDown) :: MouseUp(p, _, _) :: tail => {
        //  val parent : Option[Shape] = DOM.lookup(DOM.getShapeFrom(p))
        //  if (parent.isDefined) Some(UpdateShape(parent.get, parent.get.select))
        //  else None
        //}
        case MouseUp(point, _, ModifierKeys(_, true, _)) :: tail => { // Deselect
          //if ((box.p1 - box.p2).length < 1) {
          //  val closestShape = DOM.getShapeFrom(point)
          //  if (closestShape.distanceTo(point) < 10) closestShape deselect
          //} else {
          //  if (box.p1.x >= box.p2.x)
          //    DOM.getShapesInside(box).foreach(_ deselect)
          //  else
          //    DOM.getShapesInsideEnclosed(box).foreach(_ deselect)
          //}
          None
        }
        case MouseUp(point, _, ModifierKeys(shift, _, _)) :: tail => {
          // Deselect everything if shift isn't pressed
          if (!shift) Model deselect

//          if ((box.p1 - box.p2).length < 1) {
//            val closestShape = DOM.getShapeFrom(point)
//            if (closestShape.distanceTo(point) < 10) Some(UpdateShape(closestShape, closestShape select))
//            else None
//          } else if (box.p1.x >= box.p2.x) {
//            Some(UpdateShapes(DOM.getShapesInside(box), DOM.getShapesInside(box).map(_ select)))
//          } else {
//            Some(UpdateShapes(DOM.getShapesInsideEnclosed(box), DOM.getShapesInsideEnclosed(box).map(_ select)))
//          }
          val shapes = Model.queryForShapesWithId(box)
          if (!shapes.isEmpty)
            Select(shapes.keys)
          None
        }
        case _ => None
      }
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    val enclosed = "Color" -> "#6666DD".color
    val focused  = "Color" -> "#DD6666".color
    if (state != 'End) {
      g draw PolylineShape.fromRectangle(box).attributes_+=("Color" -> (if (isEnclosed) "#66CC66".color else "#6666CC".color)).transform(t)
    }

    boxedShapes.foreach{ s => s match {
      case s : ImmutableShape => drawShape(s)
      case s : DynamicShape => //drawShape(s.shape)
      case _ =>
    }}
    def drawShape(s : ImmutableShape) = {
      // TODO: Draw depending on whether the shape is contained or intersected
      if (isEnclosed && box.contains(s.boundary)) { // If enclosed
        g.draw(s.attributes_+=(enclosed).transform(t))
      } else if (!isEnclosed && (box.contains(s.boundary) || s.geometry.intersect(box))) { // If not enclosed
        g.draw(s.attributes_+=(enclosed).transform(t))
      } else {
        g.draw(s.attributes_+=(focused).transform(t))
      }
    }
  }

}