/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._
import com.siigna.module.endogenous.radialmenu.category._
import com.siigna.module.endogenous.radialmenu.category.{Create => MenuCreate}
import sys.Prop

/**
* The default module for the endogenous module pack. Works as access point to
* the rest of the modules.
*/
object Default extends Module {

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap     = DirectedGraph( 'Start -> 'Event -> 'Start )

  def fullScreenBox = Rectangle2D(Siigna.screen.bottomRight - Vector2D(20, -5), Siigna.screen.bottomRight - Vector2D(40, -25))

  var firstStart : Boolean = true
  var firstMenuLoad : Boolean = true

   //The nearest shape to the current mouse position.
  var nearestShape : Option[Shape] = None

  private var message : String = ""

  //The last module this module forwarded to, if any.
  var previousModule : Option[Symbol] = None

  //store the latest Key Event to be able to see whether a category (C,H,E,P, or F) was chosen before
  var previousKey :Option[Char] = None

  def stateMachine = Map( 'Start -> ((events : List[Event]) => {
    nearestShape = Model(Siigna.mousePosition)
      if (firstStart == true) {
        Siigna.display("Loading Siigna modules ver. 0.1.20.0 (442 kb")
        firstStart = false
      }
      events match {
        case MouseDown(point, MouseButtonLeft, _) :: tail           => {
          if (fullScreenBox.contains(point.transform(Siigna.physical))) {

          } else ForwardTo('Selection)
        }
        case MouseDown(point, MouseButtonRight, _) :: tail          => {
          if (firstMenuLoad == true) {
            Siigna.display("...loading modules, please wait")
            Thread.sleep(500)
            Siigna.clearDisplay()

            ForwardTo('Menu)

            //preload commonly used modules
            Preload('Polyline)
            Preload('Artline)
            Preload('Text)
            Preload('Rectangle)
            Preload('Lineardim)
            firstMenuLoad = false
          }
          else ForwardTo('Menu)
        }

        //special key inputs
        case KeyDown(('z' | 'Z'), ModifierKeys(_, true, _)) :: tail => Model.undo
        case KeyDown(('y' | 'Y'), ModifierKeys(_, true, _)) :: tail => Model.redo
        case KeyDown('a', ModifierKeys(_, true, _)) :: tail         => Model.selectAll

        //ignore mouse up events
        case KeyUp(key, _) :: tail =>

        //match key down events
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
            case 'a' => {
              if (previousKey == Some('c')) {
                Siigna.display("artline")
                ForwardTo('Artline)
                previousKey = Some('a')
              }
              else previousKey = Some('a')
            }
            //TODO: fix circle shortcut (circle does not draw)
            case 'c' => {
              if(previousKey == 'c') {
                Siigna.display("circle")
                ForwardTo('Circle)
              }
              //open the CREATE menu
              else {
                Send(Message(MenuCreate(Some(Start))))
                ForwardTo('Menu)
                previousKey = Some('c')
              }
            }
            case 'd' => {
              if(previousKey == Some('c')) {
                Siigna.display("dimension")
                ForwardTo('Lineardim)
                previousKey = Some('d')
              }
              else if(previousKey == Some('h')) {
                Siigna.display("distance?")
                ForwardTo('Distance)
                previousKey = Some('d')
              }
              else previousKey = Some('d')
            }
            //open the FILE menu
            case 'f' => {
                Send(Message(File(Some(Start))))
                ForwardTo('Menu)
                previousKey = Some('f')
            }
            //open the HELPERS menu
            case 'h' => {
                Send(Message(Helpers(Some(Start))))
                ForwardTo('Menu)
                previousKey = Some('h')
            }
            //open the MODIFY menu
            case 'm' => {
                Send(Message(Modify(Some(Start))))
                ForwardTo('Menu)
                previousKey = Some('m')
            }
            case 'l' => {
              if (previousKey == Some('c')) {
                Siigna.display("polyline")
                ForwardTo('Polyline)
                previousKey = Some('l')
              }
              else previousKey = Some('l')
            }
            //TODO: fix opening print dialog with shortcut - opens again & again (last event (p) continously evoked??)
            //case 'p' => {
            //  interface.display("opening print dialog")
            //  Thread.sleep(500)
            //  interface.clearDisplay()
            //  ForwardTo('Print)
            //}
            case 'r' => {
              if (previousKey == Some('c')) {
                Siigna.display("rectangle")
                ForwardTo('Rectangle)
                previousKey = Some('r')
              }
              else previousKey = Some('l')
            }
            //open the PROPERTIES menu
            case 'p' => {
              if(previousKey == Some('f')) {
                Siigna.display("print")
                previousKey = Some('p')
                ForwardTo('Print)
              }
              //open the PROPERTIES menu
              else {
                Send(Message(Properties(Some(Start))))
                ForwardTo('Menu)
                previousKey = Some('p')
              }
            }
            case 't' => {
              if (previousKey == Some('c')) {
                Siigna.display("text")
                ForwardTo('Text)
                previousKey = Some('t')
              }
              else previousKey = Some('t')
            }
            case _ =>
          }
        }
        case m => Log.debug("Default module received unknown input: " + m)
      }
    }))

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (nearestShape.isDefined) {
      g draw nearestShape.get.addAttribute("Color" -> "#AAAAFF".color).transform(t)
    }

    // Draw boundary
    drawBoundary(g, t)
  }

  private def drawBoundary(g : Graphics, t : TransformationMatrix) {
    def unitX(times : Int) = Vector2D(times * Siigna.paperScale, 0)

    // Get the boundary
    val boundary = Model.boundary

    // Define header
    val headerHeight = scala.math.min(boundary.height, boundary.width) * 0.025
    // Paper scale
    val scale = TextShape("Scale 1:"+Siigna.paperScale, unitX(1), headerHeight * 0.7)
    // Get URL
    val getURL = TextShape(" ", Vector2D(0, 0), headerHeight * 0.7)

    val headerWidth  = (scale.boundary.width + getURL.boundary.width) * 1.2
    val headerBoundary = Rectangle2D(boundary.bottomRight, boundary.bottomRight - Vector2D(headerWidth, -headerHeight))

    val transformation : TransformationMatrix = t.concatenate(TransformationMatrix(boundary.bottomRight - Vector2D(headerWidth * 0.99, -headerHeight * 0.8), 1))

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