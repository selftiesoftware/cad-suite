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
import com.siigna._
import com.siigna.module.base.Menu._
import com.siigna.module.base.radialmenu.category._
import com.siigna.module.base.radialmenu.category.{Create => MenuCreate}
import java.awt.Color

/**
* The default module for the base module pack. Works as access point to
* the rest of the modules.
 */

object Default extends Module {

  var activeUser : Option[String] = None
  var activeDrawing : Option[Int] = None

  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, 1.00f)

  var centerFocus : Vector2D = Vector2D(0,0)

  def fullScreenBox = Rectangle2D(View.screen.bottomRight - Vector2D(20, -5), View.screen.bottomRight - Vector2D(40, -25))

  var firstStart : Boolean = true
  var firstMenuLoad : Boolean = true

  var gridIsOn = false

  //graphics to show modules loading progress
  def loadBar(point : Int): Shape = PolylineShape(Rectangle2D(Vector2D(103,297),Vector2D(point+103,303))).setAttribute("raster" -> anthracite)
  def loadFrame : Shape = PolylineShape(Rectangle2D(Vector2D(100,294), Vector2D(500,306))).setAttribute("Color" -> "#AAAAAA".color)

  //The nearest shape to the current mouse position.
  var nearestShape : Option[(Int, Shape)] = None

  //The last module this module forwarded to, if any.
  var previousModule : Option[Symbol] = None

  //store the latest Key Event to be able to see whether a category (C,H,E,P, or F) was chosen before
  var previousKey :Option[Char] = None

  private var startTime : Option[Long] = None

  var titleFocus : Option[Vector2D] = None

  def stateMap = Map(
    'Start ->  {
      case events => {
        //on startup, for some reason this value defaults to true even though it is set to false in 'Menu. This line forces it to be false.
        com.siigna.module.base.Menu.moduleCallFromMenu = false
        val m = View.mousePosition

        if (Drawing(m).size > 0) {
          val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
          nearestShape = if (nearest._2.distanceTo(m) < 5) Some(nearest) else None
        }
        //values to be retrieved only once
        else if (firstStart == true) {
          //Preload('SetTitle, "com.siigna.module.base.file")
          //set a default drawing title
          //if (AppletParameters.getDrawingId.isDefined && AppletParameters.contributorName.isDefined && AppletParameters.clientReference.isDefined) {
          //  if(DrawingName == None) {
          //  com.siigna.app.controller.AppletParameters.saveNewDrawingName("untitled").get
          //  saveDrawingOwnerName(AppletParameters.getDrawingId.get,AppletParameters.contributorName.get,AppletParameters.clientReference.get)
          //  }
          //}

          //start the loading bar timer
          startTime =  Some(System.currentTimeMillis().toLong)

          Siigna.display("loaded base modules ver. 1.0 alpha. right-click the mouse to interact ")

          firstStart = false

          //be sure to zoom to the title only after the boundary has been set, so that the correct zoom center can be retrieved.
          //if (AppletParameeters.drawingIdReceivedAtStartup == false && titleFocus.isDefined) {
          //  Module('SetTitle)
          //}
          // TODO: Should this be used?
        }// else if (Drawing.attributes.int("id").isDefined && !titleFocus.isDefined) {
          //'Start
        //}

        // Reset the nearest shape if it has been deleted
        if (nearestShape.isDefined && !Drawing.contains(nearestShape.get._1)) nearestShape = None

        events match {
          case MouseDown(point, MouseButtonLeft, _) :: tail           => Module('Selection)
          case MouseDown(point, MouseButtonRight, _) :: tail          => {
            if (firstMenuLoad == true) {
              firstMenuLoad = false
            }
            Module('Menu)
          }
          /*
          case MouseUp(point, MouseButtonRight, _) :: tail          => {
            println("MU")
            if (firstMenuLoad == true) {
              Module('Menu)
              firstMenuLoad = false
            }
            else {
              println("OPENING MENU")
              Module('Menu)
            }
          }
          */

          //special key inputs
          case KeyDown(('z' | 'Z'), ModifierKeys(_, true, _)) :: tail => Drawing.undo()
          case KeyDown(('y' | 'Y'), ModifierKeys(_, true, _)) :: tail => Drawing.redo()
          case KeyDown('a', ModifierKeys(_, true, _)) :: tail         => Drawing.selectAll()
          case KeyDown(('c' | 'C'), ModifierKeys(_, true, _)) :: tail => {
            if (Drawing.selection.isDefined) {
              moduleCallFromMenu = true
              Module('Copy)
            }
          }

          //ignore mouse up events
          case KeyUp(key, _) :: tail =>

          //match key down events
          case KeyDown(key, _) :: tail => {
            key.toChar match {
              case Key.Backspace | Key.Delete => {
                if (Drawing.selection.isDefined) {
                  Delete(Drawing.selection.get)
                }
              }

              case Key.Escape => {
                Drawing.deselect()
              }
              case Key.Space => {
                if (previousModule.isDefined) Module(previousModule.get)
              }
              case 'a' => {
                if (previousKey == Some('c')) {
                  Siigna.display("artline")
                  Module('Artline)
                  previousKey = Some('a')
                }
                else if(previousKey == Some('h')) {
                  Siigna.display("click to measure area")
                  Module('Area)
                  previousKey = Some('a')
                }
                else previousKey = Some('a')
              }
              case 'c' => {
                if(previousKey == Some('p')) {
                  Module('ColorWheel)
                }
                else if(previousKey == Some('c')) {
                  Siigna.display("circle")
                  Module('Circle)
                  previousKey = None
                }
                //open the CREATE menu
                else {
                  Controller ! Message(MenuCreate(Some(Start)))
                  Module('Menu)
                  previousKey = Some('c')
                }
              }
              case 'd' => {
                if(previousKey == Some('c')) {
                  Siigna.display("dimension")
                  Module('Lineardim)
                  previousKey = Some('d')
                }
                else if(previousKey == Some('h')) {
                  Siigna.display("distance?")
                  Module('Distance)
                  previousKey = Some('d')
                }
                else previousKey = Some('d')
              }
              //open the FILE menu
              case 'f' => {
                if (previousKey == Some('c')) {
                  Siigna.display("fill is not available yet")
                  //Module('Fill)
                  previousKey = Some('f')
                } else {
                  Controller ! Message(File(Some(Start)))
                  Module('Menu)
                  previousKey = Some('f')
                }
              }
              //open the HELPERS menu
              case 'h' => {
                Controller ! Message(Helpers(Some(Start)))
                Module('Menu)
                previousKey = Some('h')
              }
              //open the MODIFY menu
              case 'm' => {
                if(previousKey == Some('m')) {
                  Siigna.display("move")
                  Module('Move)
                  previousKey = None
                }
                else
                {
                  Controller ! Message(Modify(Some(Start)))
                  Module('Menu)
                  previousKey = Some('m')
                }
              }
              case 'l' => {
                if (previousKey == Some('c')) {
                  Siigna.display("line")
                  Module('Line)
                  previousKey = Some('l')
                }
                else previousKey = Some('l')
              }
              //TODO: fix opening print dialog with shortcut - opens again & again (last event (p) continously evoked??)
              //case 'p' => {
              //  interface.display("opening print dialog")
              //  Thread.sleep(500)
              //  interface.clearDisplay()
              //  Module('Print)
              //}
              case 'r' => {
                if (previousKey == Some('c')) {
                  Siigna.display("rectangle")
                  Module('Rectangle)
                  previousKey = Some('r')
                }
                else if (previousKey == Some('m')) {
                  Siigna.display("rotate")
                  Module('Rotate)
                  previousKey = Some('r')
                }
                else previousKey = Some('r')
              }
              case 's' => {
                if (previousKey == Some('m')) {
                  Siigna.display("scale")
                  Module('Scale)
                  previousKey = Some('s')
                }
                else previousKey = Some('s')
              }

              //open the PROPERTIES menu
              case 'p' => {
                if(previousKey == Some('f')) {
                  Siigna.display("print")
                  previousKey = Some('p')
                  Module('Print)
                }
                else if(previousKey == Some('c')) {
                  Siigna.display("polyline")
                  previousKey = Some('p')
                  Module('Polyline)
                }
                //open the PROPERTIES menu
                else {
                  Controller ! Message(Properties(Some(Start)))
                  Module('Menu)
                  previousKey = Some('p')
                }
              }
              case 't' => {
                if (previousKey == Some('c')) {
                  Siigna.display("text")
                  Module('Text)
                  previousKey = Some('t')
                }
                else previousKey = Some('t')
              }
              case _ =>
            }
          }
          case _ => Log.debug("Default module received unknown input: " + events)
        }
      }
    })

  /**
  * In paint, all graphics that needs to be omnipresent is drawn:
  * - dynamic display of active geometry ie. objects that the mouse is hovering above (vertices / lines / text)
  * - a grid (if toggled)
  * - a boundary displaying the level of openness
  * - the drawing header
  */

  override def paint(g : Graphics, t : TransformationMatrix) {
    //set the focus of Siigna
    //centerFocus = Drawing.boundary.center.transform(t)

    //draw a loading bar when modules are loading.
    if(firstStart == true && startTime.isDefined){
      var loadingProgress = System.currentTimeMillis() - startTime.get
      g draw loadFrame
      if ((System.currentTimeMillis() - startTime.get) < 394) {
       g draw loadBar(loadingProgress.toInt)
      }
      else if ((System.currentTimeMillis() - startTime.get)> 394) {
        g draw loadBar(390)
      }
    }
    //draw a grid if toggled in through the Helpers menu
    //if(gridIsOn == true) com.siigna.module.base.helpers.Grid.paint(g : Graphics, t : TransformationMatrix)

    //draw highlighted vertices and segments that are selectable (close to the mouse)
    if (nearestShape.isDefined) {
      val shape  = nearestShape.get._2
      val part = shape.getPart(View.mousePosition)
      val points = shape.getVertices(part)
      points.foreach(p => g.draw(t.transform(p)))

      //TODO: activate this -> implement adding attributes to parts in mainline
      //g draw part.setAttributes("Color" -> "#22FFFF".color, "StrokeWidth" -> 1.0).transform(t)

    }

    // Draw boundary
    drawBoundary(g, t)
    // set title focus
    titleFocus = Some(Drawing.boundary.bottomRight.transform(t))
  }

  private def drawBoundary(g : Graphics, t : TransformationMatrix) {
    def unitX(times : Int) = Vector2D(times * Siigna.paperScale, 0)

    // Get the boundary
    val boundary = Drawing.boundary

    val br = boundary.bottomRight
    val bl = boundary.bottomLeft

    // Define header
    val headerHeight = scala.math.min(boundary.height, boundary.width) * 0.025

    // Paper scale
    val scale = TextShape("Scale 1:"+ (Siigna.paperScale), unitX(-10), headerHeight * 0.55)
    // Get URL
    val getURL = TextShape(" ", Vector2D(0, 0), headerHeight * 0.7)

    val headerWidth  = (scale.boundary.width + getURL.boundary.width) * 1.2

    val transformation : TransformationMatrix = t.concatenate(TransformationMatrix(boundary.bottomRight - Vector2D(headerWidth * 0.99, -headerHeight * 0.8), 1))

    val oversize1 = (boundary.bottomLeft + Vector2D(-2 * Siigna.paperScale, -2 * Siigna.paperScale))
    val oversize2 = (boundary.topRight + Vector2D(2 * Siigna.paperScale, 2 * Siigna.paperScale))

    //draw frame to indicate level of openness:
    g draw PolylineShape(Rectangle2D(oversize1, oversize2)).transform(t).setAttributes("Color" -> new Color(0.25f, 0.85f, 0.25f, 0.20f), "StrokeWidth" -> 4.0)

    // Draw horizontal headerborder
    g draw LineShape(br + Vector2D(0,(6*(Siigna.paperScale))), Vector2D((br.x/2 + bl.x),br.y) + Vector2D(0,(6*(Siigna.paperScale)))).setAttribute("StrokeWidth" -> 0.3).transform(t)

    //Draw vertical headerborder
    g draw LineShape(Vector2D((br.x/2 + bl.x),br.y), Vector2D((br.x/2 + bl.x),br.y) + Vector2D(0,(6*(Siigna.paperScale)))).setAttribute("StrokeWidth" -> 0.3).transform(t)

    //g draw separator
    g.draw(getURL.transform(transformation.translate(scale.boundary.topRight + unitX(4))))

    //draw paperScale
    g.draw(scale.transform(transformation))

    //TODO: letter width: 50% letter spacing: 200%

    // Draw ID and title
    //if (!SetTitle.text.isEmpty) {
    //  val title = TextShape(SetTitle.text, unitX(-72), headerHeight * 0.7)
    //  g draw(title.transform(transformation))
    //}

    //draw title
    //if (Drawing.attributes.int("title").isDefined && (Drawing.attributes.string("title").get.length() > 0) && SetTitle.text.isEmpty) {
    //  val title = TextShape(Drawing.attributes.string("title").get, unitX(-72), headerHeight * 0.7)
    //  g draw(title.transform(transformation))
    //}
    //draw ID
    if (Drawing.attributes.long("id").isDefined) {
      val id = TextShape("ID: "+Drawing.attributes.long("id").get, unitX(-28), headerHeight * 0.7, Attributes("Color" -> "#AAAAAA".color))
      g draw(id.transform(transformation))
    }
    if (Siigna.user.name.length() > 0) {
      val contributor = TextShape("author: "+Siigna.user.name, unitX(-120), headerHeight * 0.7)
      g draw(contributor.transform(transformation))
    }
  }
}