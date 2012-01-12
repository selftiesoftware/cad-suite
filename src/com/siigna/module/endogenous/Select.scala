/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Select extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  var box = Rectangle2D(Vector2D(0, 0), Vector2D(0, 0))

  var closeToObjectOnStart = false

  //TODO: Temporary! Create a dynamic model...!
  var selectedShape : Option[Shape] = None
  var startCoordinate : Option[Vector2D] = None
  var boxedShapes : Iterable[Shape] = Iterable()
  var currentPoint : Option[Vector2D] = None

  // should be: def isEnclosed : Boolean = (box.p1.x <= box.p2.x)
  def isEnclosed : Boolean = (startCoordinate.get.x <= currentPoint.get.x)

  def stateMap     = DirectedGraph(
    'Start -> 'MouseMove -> 'Box,
    'Start -> 'MouseDrag -> 'Box,
    'Start -> 'MouseUp   -> 'End,
    'Start -> 'KeyEscape -> 'End,
    'Box   -> 'MouseUp -> 'End,
    'Box   -> 'KeyEscape -> 'End,
    'Box   -> 'KeyEscape -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case (_ : MouseUp) :: (_ : MouseDown) :: tail => Goto('End)
        case MouseDown(point, _, _) :: tail => {
          //set the start of the selection box
          startCoordinate = Some(point)
          selectedShape = Model(point)
          if (selectedShape.isDefined && selectedShape.get.distanceTo(point) < 10)
            closeToObjectOnStart = true
          else
            box = Rectangle2D(point, point)
        }
        case MouseUp(point, _, _) :: tail => {
          startCoordinate = Some(point)
        }
        case _ =>
      }
      None
    }),
    'Box   -> ((events : List[Event]) => {
      events match {
        case MouseDrag(point, _, _) :: tail => {
          currentPoint = Some(point)
          box = Rectangle2D(startCoordinate.get, Vector2D(point.x,point.y))
        }

         //TODO: add  if KeyDown(Key.Delete, _) :: tail => delete(List[Shapes])

          //if (closeToObjectOnStart && selectedShape.isDefined) {
          //  Goto('End)
          //  ForwardTo('Move)
          //} else
          //box = Rectangle2D(Vector2D(startCoordinate, point))
          //println("mouseDrag")
          //box = Rectangle2D(startCoordinate.get,Vector2D(point.x,point.y))
        //}
        case _ =>
      }
      boxedShapes = Model(box)
      //Create ...
      println(boxedShapes)
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
        }
        case MouseUp(point, _, ModifierKeys(shift, _, _)) :: tail => {
          // Deselect everything if shift isn't pressed
          //if (!shift) Model deselect

//          if ((box.p1 - box.p2).length < 1) {
//            val closestShape = DOM.getShapeFrom(point)
//            if (closestShape.distanceTo(point) < 10) Some(UpdateShape(closestShape, closestShape select))
//            else None
//          } else if (box.p1.x >= box.p2.x) {
//            Some(UpdateShapes(DOM.getShapesInside(box), DOM.getShapesInside(box).map(_ select)))
//          } else {
//            Some(UpdateShapes(DOM.getShapesInsideEnclosed(box), DOM.getShapesInsideEnclosed(box).map(_ select)))
//          }
          //val shapes = Model.queryForShapesWithId(box)
          //if (!shapes.isEmpty)
          //  Select(shapes.keys)
        }
        case _ =>
      }
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    val enclosed = "Color" -> "#9999FF".color
    val focused  = "Color" -> "#FF9999".color
    if (state != 'End) {
      g draw PolylineShape.fromRectangle(box).addAttribute("Color" -> (if (isEnclosed) "#88AA88".color else "#8888AA".color)).transform(t)
    }

    boxedShapes.foreach{ s => s match {
      case s : ImmutableShape => drawShape(s)
      case s : DynamicShape => //drawShape(s.shape)
      case _ =>
    }}
    def drawShape(s : ImmutableShape) = {
      // TODO: Draw depending on whether the shape is contained or intersected
      if (isEnclosed && box.contains(s.boundary)) {
        // If enclosed
        g.draw(s.addAttribute(enclosed).transform(t))
      }
      //todo: fix this:----> else if (!isEnclosed && (box.contains(s.boundary) || s.geometry.intersections(box))) {
      //If not enclosed
      //g.draw(s.addAttribute(enclosed).transform(t))
      //}

      else {
        g.draw(s.addAttribute(enclosed).transform(t))
      }
    }
  }
}