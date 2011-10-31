/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._
import java.awt.{Toolkit, GraphicsEnvironment, Color}

/**
* The default module for the endogenous module pack. Works as access point to
* the rest of the modules.
*/
object Default extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph( 'Start -> 'Event -> 'Start )

  def fullScreenBox = Rectangle(Siigna.screen.bottomRight - Vector(20, -5), Siigna.screen.bottomRight - Vector(40, -25))

  def asInstanceOfOpt[B : scala.reflect.AnyValManifest](obj: Any) : Option[B] =
        if (obj.isInstanceOf[B])
          Some(obj.asInstanceOf[B])
        else
          None

  /**
   * The nearest shape to the current mouse position.
   */
  var nearestShape : Option[Shape] = None

  /**
   * The last module this module forwarded to, if any.
   */
  var previousModule : Option[Symbol] = None

  lazy val stateMachine = Map( 'Start -> ((events : List[Event]) => {
      nearestShape = Model(Siigna.mousePosition)
      events match {
        case MouseDown(point, MouseButtonLeft, _) :: tail           => {
          if (fullScreenBox.contains(point.transform(Siigna.physical))) {

          } else ForwardTo('Select)
        }
        case MouseDown(point, MouseButtonRight, _) :: tail          => ForwardTo('Menu)
        case KeyDown(('z' | 'Z'), ModifierKeys(_, true, _)) :: tail => Model.undo
        case KeyDown(('y' | 'Y'), ModifierKeys(_, true, _)) :: tail => Model.redo
        case KeyDown('a', ModifierKeys(_, true, _)) :: tail         => Model.selectAll
        case KeyDown(key, _) :: tail => {
          key.toChar match {
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
    if (nearestShape.isDefined) {
      g draw nearestShape.get.attributes_+=("Color" -> "#AAAAFF".color).transform(t)
    }

    // Draw boundary
    drawBoundary(g, t)
  }

  private def drawBoundary(g : Graphics, t : TransformationMatrix) {
    def unitX(times : Int) = Vector(times * Siigna.paperScale, 0)

    // Get the boundary
    val boundary = Model.boundary

    // Define header
    val headerHeight = scala.math.min(boundary.height, boundary.width) * 0.025
    // Paper scale
    val scale = TextShape("Scale 1:"+Siigna.paperScale, unitX(1), headerHeight * 0.7)
    // Get URL
    val getURL = TextShape(" ", Vector(0, 0), headerHeight * 0.7)

    val headerWidth  = (scale.boundary.width + getURL.boundary.width) * 1.2
    val headerBoundary = Rectangle(boundary.bottomRight, boundary.bottomRight - Vector(headerWidth, -headerHeight))

    val transformation : TransformationMatrix = t.concatenate(TransformationMatrix(boundary.bottomRight - Vector(headerWidth * 0.99, -headerHeight * 0.8), 1))

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