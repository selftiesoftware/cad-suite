/* 2011 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna.app.view.Graphics
import java.awt.Color

//import app.view.filter.GaussianBlur
import com.siigna.module.endogenous.radialmenu._
import com.siigna._

import category._

/**
 * The menu module. This module shows the menu as radial items and categories in 13 places (directions):
 * N, NNE, ENE, E, ESE, SSE, S, SSW, WSW, W, WNW and NNW. There is also a center (C) place.
 */
object Menu extends Module {

  // The event handler
  lazy val eventHandler = new EventHandler(RadialMenuStateMap, stateMachine)

  // The center of the wheel
  private var center : Option[Vector]         = None

  // The current active category
  private var currentCategory : MenuCategory  = Start

  // The distance to draw the icons; Used in the initiation of the menu module to animate the icons.
  private var distanceScale : Double = 1

  // The position of the mouse at any given time
  private var mousePosition  = Vector(0, 0)

  // The transformation to use throughout the paint
  private var transformation = TransformationMatrix(Vector(0, 0), 1)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseDown(point, MouseButtonRight, _) :: tail => {
          // Initialize the menu
          center          = Some(point)
          currentCategory = Start

          // Make sure the rest of the program doesn't move
          Siigna.navigation = false

          // Disable tracking and snapping
          eventParser.disable
        }
        case _ => Goto('InteractionTest, false)
      }
    }),
    'InteractionTest -> ((events : List[Event]) => {
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseUp(point, _, _) :: tail => {
          val direction = this.direction(point)
          val level     = if (this.distance < 68) 3 else if (this.distance < 102) 2 else 1

          // Make the interaction!
          interact(currentCategory, direction, 1, level) match {
            case Some(category : MenuCategory) => currentCategory = category
            case Some(item : MenuItem)         => {
                if (item.module != 'None) {
                  Goto('End)
                  ForwardTo(item.module)
                }
            }
            case None => if (level == 3) currentCategory = Start
          }
        }
        case MouseMove(p, _, _) :: tail => mousePosition = p
        case MouseWheel(point, _, _, delta) :: tail => {

          val direction = this.direction(point)
          val level     = if (this.distance < 68) 3 else if (this.distance < 102) 2 else 1

          val deltaLevel = if (level + delta < 1) 1 else if (level + delta > 3) 3 else delta + level

          // Make the interaction!
          interact(currentCategory, direction, 1, deltaLevel) match {
            case Some(category : MenuCategory) => currentCategory = category
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
        case MouseDrag(point, _,_) :: tail => mousePosition = point
        case _ => {}
      }
    }),
    'End -> ((events : List[Event]) => {
      // Set everything back to normal
      Siigna.navigation = true
      center = None
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
      val menuCenter = if (center.isDefined) transformation.scale(1).transform(center.get) else Vector(0, 0)
      val t = TransformationMatrix(menuCenter, 1 * distanceScale).flipY

      // Draws a Category at a given level
      def drawCategory(category : MenuCategory, scale : Double = 1) {
        // Determines whether the given menu-event is active.
        def isActive(event : MenuEvent) = (distance > (100 * scale) && distance < (160 * scale) && this.direction(mousePosition) == event)

        // Determines whether the current event is active and returns a set of
        // attributes accordingly.
        def getAttr(event : MenuEvent) =
          if (isActive(event))
            "Color" -> "#444444".color
          else
            "Color" -> "#999999".color

        // Gets a transformation matrix from a given event.
        // TODO: Refactor!
        def getT(event : MenuEvent) = event match {
          case EventC => t.translate(Vector(-10000, -10000))
          case _      => t.scale(scale).translate(event.vector * 129)
        }

        // Draws a MenuCategory icon and the background-icon for the given direction.
        def drawCategoryItem(item : MenuCategory, event : MenuEvent, newT : TransformationMatrix) {

          // Category Backgrounds
          val rotation = event.vector.angle + 90
          def backfillVectors= RadialMenuIcon.BackFill.map(_.transform(newT.rotate(rotation)))
          val backfillScreenX = backfillVectors.map(_.x.toInt).toArray
          val backfillScreenY = backfillVectors.map(_.y.toInt).toArray

          // Draw the background for the category.
          g setColor item.color
          if (scale == 1) {
            g.g.fillPolygon(backfillScreenX, backfillScreenY, backfillVectors.size)
          } else if (item == currentCategory) {
            g.g.fillPolygon(backfillScreenX, backfillScreenY, backfillVectors.size)
          }


          // Fills the background-color for the categories (circle).
          if (event != EventC) {
            val center = Vector(0,0).transform(newT.rotate(rotation))
            val size = (52 * scale * distanceScale).toInt
            val half = (size * 0.5).toInt
            g.g.fillArc(center.x.toInt -  half, center.y.toInt - half, size, size, 0, 360)
          }

          // Draw Category Description (C,H,E,P letter) in inner circles.
          if (item != currentCategory) {
            event.icon.foreach(s => g.draw(s.attributes_+=(getAttr(event)).transform(newT)))
            if (scale < 1)
              g draw TextShape(item.name.substring(0, 1), Vector(0, 0), newT.scaleFactor * 24, Attributes("TextAlignment" -> Vector(0.5, 0.5))).transform(newT)
            else
              g draw TextShape(item.name, Vector(0, 0), newT.scaleFactor * 9, Attributes("TextAlignment" -> Vector(0.5, 0.5))).transform(newT)
          } else {
              g draw TextShape(item.name.substring(0, 1), Vector(0, 0), newT.scaleFactor* 44, Attributes("TextAlignment" -> Vector(0.5, 0.5))).transform(newT)
          }

        }

        // Draws a MenuItem and the background-icon for the given direction.
        def drawItem(item : MenuItem, event : MenuEvent, newT : TransformationMatrix) : Unit = {
          // Icon Background
          val rotation2 = event.vector.angle + 30
          val fillVectors = RadialMenuIcon.IconFill.map(_.transform(newT.rotate(rotation2, Vector(0,0))))
          val fillScreenX = fillVectors.map(_.x.toInt).toArray
          val fillScreenY = fillVectors.map(_.y.toInt).toArray

          // Draw the background for the menu item
          g setColor RadialMenuIcon.itemColor
          g.g.fillPolygon(fillScreenX, fillScreenY, fillVectors.size)

          // Icons
          item.icon.foreach(s => g.draw(s.transform(newT)))
          event.icon.foreach(s => {
            g.draw(s.attributes_+=(getAttr(event)).transform(newT))
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
            g draw TextShape(tooltip, menuCenter + Vector(0, 170), 10).attributes_+=("TextAlignment" -> Vector(0.5, 0))
          }
        })
        // Draws the parent category recursively
        if (category.parent.isDefined) {
          drawCategory(category.parent.get, scale * 0.65)
        }
      }

      // Draw the center element
      if (currentCategory.C.isDefined) {
        g draw TextShape(currentCategory.C.get.name, Vector(0, 0), 12, Attributes("Color" -> "#333333".color, "TextAlignment" -> Vector(0.5, 0.5))).transform(t)
        g draw CircleShape(Vector(0, 0), Vector(0, 26)).transform(t)
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
//        g draw TextShape(tooltip, menuCenter + Vector(0, 170), 10).attributes_+=("TextAlignment" -> Vector(0.5, 0))
//      }

      // Draws the first category
      drawCategory(currentCategory)

      // Draw the outlines of the categories in the four corners of the world
      RadialMenuIcon.NOutline.foreach(s => g.draw(s.attributes_+=("Color" -> "#CCCCCC".color).transform(t)))
      RadialMenuIcon.WOutline.foreach(s => g.draw(s.attributes_+=("Color" -> "#CCCCCC".color).transform(t)))
      RadialMenuIcon.SOutline.foreach(s => g.draw(s.attributes_+=("Color" -> "#CCCCCC".color).transform(t)))
      RadialMenuIcon.EOutline.foreach(s => g.draw(s.attributes_+=("Color" -> "#CCCCCC".color).transform(t)))
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
  def direction(point : Vector) = {
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
    if (center.isDefined) (center.get - mousePosition).length * Siigna.zoom else java.lang.Double.POSITIVE_INFINITY
  }

}
