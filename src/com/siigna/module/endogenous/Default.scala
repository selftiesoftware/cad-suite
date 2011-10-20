/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import java.awt.Color

import com.siigna._

/**
* The default module for the endogenous module pack. Works as access point to
* the rest of the modules.
*/
class Default extends Module {

  var mousePosition = Vector(0, 0)

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph( 'Start -> 'Event -> 'Start )

  /**
   * The nearest shape to the current mouse position.
   */
  var nearestShape : Option[Shape] = None

  /**
   * The last module this module forwarded to, if any.
   */
  var previousModule : Option[Symbol] = None

  lazy val stateMachine = Map( 'Start -> ((events : List[Event]) => {
      nearestShape = Model(mousePosition)
      events match {
        case MouseMove(point, _, _) :: tail                         => mousePosition = point
        case MouseDown(point, MouseButtonLeft, _) :: tail           => ForwardTo('Select)
        case MouseDown(point, MouseButtonRight, _) :: tail          => ForwardTo('Menu)
        case KeyDown(('z' | 'Z'), ModifierKeys(_, true, _)) :: tail => Model.undo
        case KeyDown(('y' | 'Y'), ModifierKeys(_, true, _)) :: tail => Model.redo
        case KeyDown('a', ModifierKeys(_, true, _)) :: tail         => Model.selectAll
        case KeyDown(key, _) :: tail => {
          key match {
            case Key.Backspace | Key.Delete => {
              if (Model.isSelected) {
                val selected = Model.selected
                Model.deselect
                Delete(selected)
              }
            }
            case Key.Control => {
              if (Model.isSelected) ForwardTo('Copy)
            }
            case Key.Escape => {
              Model.deselect
            }
            case Key.Space => {
              if (previousModule.isDefined) ForwardTo(previousModule.get)
            }
            case 'a' => ForwardTo('Arc)
            case 'l' => ForwardTo('Polyline)
            case 't' => ForwardTo('Text)
            case _ =>
          }
        }
        case m => Log.debug("Default module received unknown input: " + m)
      }
    }))

  override def paint(g : Graphics, t : TransformationMatrix) {
    def unitX(times : Int) = Vector(times * Siigna.paperScale, 0)

    if (nearestShape.isDefined && nearestShape.get.distanceTo(mousePosition) < 5) {
      g draw nearestShape.get.attributes_+=("Color" -> "#AAAAFF".color).transform(t)
    }

    // Get the boundary
    val boundary = Model.boundary

    // Define header
    val headerHeight = scala.math.min(boundary.height, boundary.width) * 0.025
    // Paper scale
    val scale = TextShape("Scale 1:"+Siigna.paperScale, unitX(1), headerHeight * 0.7)
    // Get URL
    //val getURL = TextShape("Get URL", Vector(0, 0), headerHeight * 0.7)
    val getURL = TextShape(" ", Vector(0, 0), headerHeight * 0.7)

    val headerWidth  = (scale.boundary.width + getURL.boundary.width) * 1.2
    val headerBoundary = Rectangle(boundary.bottomRight, boundary.bottomRight - Vector(headerWidth, -headerHeight))

    val transformation : TransformationMatrix = t.concatenate(TransformationMatrix(boundary.bottomRight - Vector(headerWidth * 0.99, -headerHeight * 0.8), 1))

    //val rectangle = PolylineShape.fromRectangle(headerBoundary).transform(t).setAttributes("Color" -> color)
    val separator = LineShape(Vector(scale.transform(transformation.translate(unitX(2))).boundary.topRight.x, headerBoundary.transform(t).topLeft.y),
                              Vector(scale.transform(transformation.translate(unitX(2))).boundary.topRight.x, headerBoundary.transform(t).bottomLeft.y))

    // Draw header
    g draw LineShape(headerBoundary.topLeft, headerBoundary.topRight)
    g draw LineShape(headerBoundary.topRight, headerBoundary.bottomRight)
    g draw LineShape(headerBoundary.bottomRight, headerBoundary.bottomLeft)
    g draw LineShape(headerBoundary.bottomLeft, headerBoundary.topLeft)
    //g draw separator
    g.draw(scale.transform(transformation))
    g.draw(getURL.transform(transformation.translate(scale.boundary.topRight + unitX(4))))
  }

}