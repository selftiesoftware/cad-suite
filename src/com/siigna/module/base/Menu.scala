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

package com.siigna.module.base

import com.siigna.app.view.Graphics
import java.awt.Color

//import app.view.filter.GaussianBlur
import com.siigna.module.base.radialmenu._

import com.siigna._

import category._
import com.siigna.module.base.radialmenu.category.{Create => MenuCreate}

/**
 * The menu module. This module shows the menu as radial items and categories in 13 places (directions):
 * N, NNE, ENE, E, ESE, SSE, S, SSW, WSW, W, WNW and NNW. There is also a center (C) place.
 */
object Menu extends Module {

  // The event handler
  def eventHandler = new EventHandler(RadialMenuStateMap, stateMachine)

  // The center of the radial menu
  var center : Option[Vector2D]         = None

  // The current active category
  private var currentCategory : MenuCategory  = Start

  // The distance to draw the icons; Used in the initiation of the menu module to animate the icons.
  private var distanceScale : Double = 1

  //defining the parameters needed to start the menu
  def initializeMenu() {

    // Make sure the rest of the program doesn't move
    Siigna.navigation = false

    // Disable tracking and snapping
    eventParser.disable
  }

  //a var to pass on the last key down back to Default, to check if it is needed to activate a shortcut
  var lastKey : Option[KeyDown] = None

  var moduleCallFromMenu : Boolean = false

  // the center after the radial menu is closed. Used if other modules need to know where it was (used in Color Wheel)
  var oldCenter = Vector2D(0 ,0)

  /**
   * The radius of the wheel. Can be adjusted to adjust the size of the wheel.
   */
  var radius = 180

  // The transformation to use throughout the paint
  private var transformation = TransformationMatrix(Vector2D(0, 0), 1)

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        //if a Menu Category is received as a message from the Default module, display that category
        case Message(category : MenuCategory) :: tail => {
          center = Some(Vector2D(0, 0))
          currentCategory = category
          initializeMenu()
        }
        case Message(point : Vector2D) :: tail => {
          //call the menu:
          val p = point.transform(View.physical.flipY)
          // Move the menu if the distance from the center to the screen
          // is less than the radius of the menu
          val d : Vector2D = if (View.screen.distanceTo(p) < radius) {
            val dBot = View.screen.borderBottom.distanceTo(p)
            val dTop = View.screen.borderTop.distanceTo(p)
            val dLeft = View.screen.borderLeft.distanceTo(p)
            val dRight = View.screen.borderRight.distanceTo(p)
            Vector2D(
                   if (dLeft < dRight && dLeft < radius)  radius - dLeft
              else if (dLeft > dRight && dRight < radius) dRight - radius
              else 0,
                   if (dTop < dBot && dTop < radius) dTop - radius
              else if (dTop > dBot && dBot < radius) radius - dBot
              else 0)
          } else Vector2D(0, 0)

          center = Some(point + d)
          currentCategory = Start
          initializeMenu()
        }
        case _ => Goto('InteractionTest, false)
      }
    }),
    'InteractionTest -> ((events : List[Event]) => {
      events match {
        //forward to the 'End and then Default menu if a key is pressed
        case KeyUp(key, _) :: (e : KeyDown) :: tail => {
          lastKey = Some(e)
          Goto('End)
        }

        case MouseDown(_, MouseButtonRight, _) :: tail => {
          Goto('End, false)
        }
        case MouseUp(point, _, _) :: tail => {
          val direction = this.direction(point)
          val level     = if (this.distance < 68) 3 else if (this.distance < 102) 2 else 1

          // Make the interaction!
          interact(currentCategory, direction, 1, level) match {
            case Some(category : MenuCategory) => currentCategory = category
            case Some(item : MenuItem)         => {
                if (item.module != 'None) {
                //set a flag to destinguish between mouse-induced and menu-induced module calls. (Used in 'Move)
                 moduleCallFromMenu = true
                  Goto('End)
                  Preload(item.module, item.modulePath)
                  ForwardTo(item.module)
                  // Save the previous module
                  Default.previousModule = Some(item.module)
                }
            }
            case None => if (level == 3) currentCategory = Start
          }
        }
        case MouseWheel(point, _, _, delta) :: tail => {

          val direction = this.direction(point)
          val level     = if (this.distance < 68) 3 else if (this.distance < 102) 2 else 1

          val deltaLevel = if (level + delta < 1) 1 else if (level + delta > 3) 3 else delta + level

          // Make the interaction!
          interact(currentCategory, direction, 1, deltaLevel.toInt) match {
            case Some(category : MenuCategory) => {
              currentCategory = category
            }
            case Some(item : MenuItem)         => {
                if (item.module != 'None) {
                  Goto('End)
                  ForwardTo(item.module)
                }
            }
            case None => if (level == 3) currentCategory = Start
          }
        }
        case _ =>
      }
    }),
    'InteractionTestDrag -> ((events : List[Event]) => {
      events match {
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ => {}
      }
    }),
    'End -> ((events : List[Event]) => {
      // Set everything back to normal
      Siigna.navigation = true
      if(center.isDefined)
        oldCenter = center.get
      center = None
      if(lastKey.isDefined)
        Controller ! lastKey.get
      lastKey = None
    })
  )

  /**
   * Paints the menu. If the menu is being initalized we multiply the transformationMatrix with a <code>distanceScale</code> to show an animation
   * of the icons, origining from the center.
   */
  override def paint(g : Graphics, transformation : TransformationMatrix) {
    // Saves a transformationMatrix, that is fixed to the center and
    // independent of the zoom-level, since we don't want our menu
    // to scale up and down.
    if (center.isDefined) {
      this.transformation = transformation
      val menuCenter = if (center.isDefined) transformation.scale(1).transform(center.get) else Vector2D(0, 0)
      val t = TransformationMatrix(menuCenter, 1 * distanceScale).flipY

      // Draws a Category at a given level
      def drawCategory(category : MenuCategory, scale : Double = 1) {
        // Determines whether the given menu-event is active.
        def isActive(event : MenuEvent) = (distance > (100 * scale) && distance < (160 * scale) && this.direction(mousePosition) == event)

        // Determines whether the current event is active and returns a set of
        // attributes accordingly.
        def getAttr(event : MenuEvent) =
          if (isActive(event))
            "Color" -> "#222222".color
          else
            "Color" -> "#BBBBBB".color

        // Gets a transformation matrix from a given event.
        // TODO: Refactor!
        def getT(event : MenuEvent) = event match {
          case EventC => t.translate(Vector2D(-10000, -10000))
          case _      => t.scale(scale).translate(event.vector * (radius - 50))
        }

        // Draws a MenuCategory icon and the background-icon for the given direction.
        def drawCategoryItem(item : MenuCategory, event : MenuEvent, newT : TransformationMatrix) {

          // Category Backgrounds
          val rotation = event.vector.angle + 90
          def backfillVector2Ds = RadialMenuIcon.BackFill.map(_.transform(newT.rotate(rotation)))
          val backfillScreenX = backfillVector2Ds.map(_.x.toInt).toArray
          val backfillScreenY = backfillVector2Ds.map(_.y.toInt).toArray

          // Draw the background for the category.
          g setColor item.color
          if (scale == 1) {
            g.g.fillPolygon(backfillScreenX, backfillScreenY, backfillVector2Ds.size)
          } else if (item == currentCategory) {
            g.g.fillPolygon(backfillScreenX, backfillScreenY, backfillVector2Ds.size)
          }


          // Fills the background-color for the categories (circle).
          if (event != EventC) {
            val center = Vector2D(0,0).transform(newT.rotate(rotation))
            val size = (52 * scale * distanceScale).toInt
            val half = (size * 0.5).toInt
            g.g.fillArc(center.x.toInt -  half, center.y.toInt - half, size, size, 0, 360)
          }

          // Draw Category Description (C,H,E,P letter) in inner circles.
          if (item != currentCategory) {
            event.icon.foreach(s => g.draw(s.addAttribute(getAttr(event)).transform(newT)))
            if (scale < 1)
              //draws the first letter of the inactive menus in the inner circle
              g draw TextShape(item.name.substring(0, 1), Vector2D(0, 0), newT.scaleFactor * 44, Attributes("TextAlignment" -> Vector2D(0.5, 0.5), "Color" -> "#777777".color)).transform(newT)
            else
              //draws menu titles for menus with a subcategory only
              g draw TextShape(item.name, Vector2D(0, 0), newT.scaleFactor * 9, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))).transform(newT)
          }
          else {
              //draws the first letter of the active menu in the inner circle
              g draw TextShape(item.name.substring(0, 1), Vector2D(0, 0), newT.scaleFactor* 44, Attributes("TextAlignment" -> Vector2D(0.5, 0.5))).transform(newT)
          }
        }

        // Draws a MenuItem and the background-icon for the given direction.
        def drawItem(item : MenuItem, event : MenuEvent, newT : TransformationMatrix) : Unit = {
          // Icon Background
          val rotation2 = event.vector.angle + 30
          val fillVector2Ds = RadialMenuIcon.IconFill.map(_.transform(newT.rotate(rotation2, Vector2D(0,0))))
          val fillScreenX = fillVector2Ds.map(_.x.toInt).toArray
          val fillScreenY = fillVector2Ds.map(_.y.toInt).toArray

          // Draw the background for the menu item
          g setColor RadialMenuIcon.itemColor
          g.g.fillPolygon(fillScreenX, fillScreenY, fillVector2Ds.size)

          // Icons
          item.icon.foreach(s => g.draw(s.transform(newT)))
          event.icon.foreach(s => {
            g.draw(s.addAttribute(getAttr(event)).transform(newT))
          })
        }

        // Draws the current category
        category.stateMap.filter(_._2 != None).foreach( eventAndItem => {
          val item  = eventAndItem._2
          val event = eventAndItem._1._2
          if (item.isInstanceOf[MenuItem] && scale == 1) drawItem(item.asInstanceOf[MenuItem], event, getT(event))
          else if (item.isInstanceOf[MenuCategory] ) drawCategoryItem(item.asInstanceOf[MenuCategory], event, getT(event))

          // Draws the tooltip
          if (isActive(event)) {
            val tooltip = item match {
              case item : MenuItem => item.module.name
              case category : MenuCategory => category.name
              case _ => " "
            }
            g draw TextShape(tooltip, menuCenter + Vector2D(0, -40), 10).addAttribute("TextAlignment" -> Vector2D(0.5, 0))
          }
        })
        // Draws the parent category recursively
        if (category.parent.isDefined) {
          drawCategory(category.parent.get, scale * 0.65)
        }
      }

      // Draw the center element
      if (currentCategory.C.isDefined) {
        g draw TextShape(currentCategory.C.get.name, Vector2D(0, 0), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector2D(0.5, 0.5))).transform(t)
        g draw CircleShape(Vector2D(0, 0), Vector2D(0, 26)).transform(t)
      }

      // Draws the tooltip to show the user which category/module is active
//      val activeElement = currentCategory.stateMap.apply(currentCategory, direction)
//      if (activeElement.isDefined && distance >= 68 && distance <= 300) {
//        def getCategoryTooltip(category : MenuCategory) : String = {
//          val parent = category.parent
//          if (parent.isDefined) getCategoryTooltip(parent.get) + category.name + " - "
//          else if (category.name != "Start") category.name + " - "
//          else ""
//        }
//        val tooltip = getCategoryTooltip(currentCategory) + (activeElement.get match {
//          case item : MenuItem => item.module.name
//          case category : MenuCategory => category.name
//          case _ => ""
//        })
//        g draw TextShape(tooltip, menuCenter + Vector2D(0, 170), 10).attributes_+=("TextAlignment" -> Vector2D(0.5, 0))
//      }

      // Draws the first category
      drawCategory(currentCategory)

      // Draw the outlines of the categories in the four corners of the world
      RadialMenuIcon.NOutline.foreach(s => g.draw(s.addAttribute("Color" -> "#CCCCCC".color).transform(t)))
      RadialMenuIcon.WOutline.foreach(s => g.draw(s.addAttribute("Color" -> "#CCCCCC".color).transform(t)))
      RadialMenuIcon.SOutline.foreach(s => g.draw(s.addAttribute("Color" -> "#CCCCCC".color).transform(t)))
      RadialMenuIcon.EOutline.foreach(s => g.draw(s.addAttribute("Color" -> "#CCCCCC".color).transform(t)))
    }
  }

  def interact(category : MenuCategory, direction : MenuEvent, currentLevel : Int, lastLevel : Int) : Option[MenuElement] = {
    if (lastLevel == currentLevel) {
      category.stateMap(category -> direction)
    } else if (category.parent.isDefined)
      interact(category.parent.get, direction, currentLevel + 1, lastLevel)
    else if (this.distance < 40 && category.C.isDefined)
      interact(category.C.get, EventC, currentLevel + 1, lastLevel)
    else
      None
  }

  /**
   * Returns the direction in terms of MenuEvents, calculated from the angle
   * of a given point to the current center of the radial menu.
   */
  def direction(point : Vector2D) = {
    if (center.isDefined) {
      val angle  = (point - center.get).angle
      if      (angle > 345 || angle < 15)   EventE
      else if (angle > 15  && angle < 45)   EventENE
      else if (angle > 45  && angle < 75)   EventNNE
      else if (angle > 75  && angle < 105)  EventN
      else if (angle > 105 && angle < 135)  EventNNW
      else if (angle > 135 && angle < 165)  EventWNW
      else if (angle > 165 && angle < 195)  EventW
      else if (angle > 195 && angle < 225)  EventWSW
      else if (angle > 225 && angle < 255)  EventSSW
      else if (angle > 255 && angle < 285)  EventS
      else if (angle > 285 && angle < 315)  EventSSE
      else if (angle > 315 && angle < 345)  EventESE
      else                                  MenuEventNone
    } else MenuEventNone
  }

  /**
   * Calculates the distance from the center to the current position of
   * the mouse.
   */
  def distance : Double = {
    if (center.isDefined) (center.get - mousePosition).length * View.zoom else java.lang.Double.POSITIVE_INFINITY
  }

}
