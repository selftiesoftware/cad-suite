/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.helpers

import com.siigna._
import com.siigna.util.event.End
import java.awt.MediaTracker
import module.cad.create.{InputRequest, DynamicDrawFromVector2D}
import module.Tooltip
import scala.Some
import javax.swing.filechooser.FileNameExtensionFilter

/**
 * A module to
 * a) import a background image and place it on a drawing
 * b) remove a background image from a drawing
 */

class ImageBackground extends Module {

  //file filter for the import dialog
  lazy val JPGFileFilter = new FileNameExtensionFilter("JPG files", "jpg")

  var image : Option[java.awt.Image] = None

  //TODO: tie the image to a rectangleShape so that manipulating that shape also alters the image

  //the points specifying the location of the background image
  var points = List[Vector2D]()

  //a tracker to check when the image is loaded (the image height and width can not be read before then)
  //TODO: is it cool to instantiate a new applet ?!  - anyway it was a way to get the tracker working.
  // - No it is not cool. In fact it is impossible since I've refactored the applet away from mainline.
  //   I've changed it to null. If there are any issues this might be it :)
  var tracker : MediaTracker = new MediaTracker(null)

  //on the basis of two points, update the Y-coordinate of the last point "v" to match a given proportion.
  def parse(ratio : Double, p : Vector2D, v : Vector2D) : Vector2D = {
    val width = v.x - p.x
    val vY = width / ratio
    Vector2D(v.x,p.y + vY)
  }

  var proportions = 1.0

  val stateMap: StateMap = Map(

    'Start -> {
      //exit mechanisms
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      case End(v : Vector2D) :: tail => {

        //use the first point
        if (points.length == 0){

          points = points :+ v
          val vector2DGuide = DynamicDrawFromVector2D((v: Vector2D) => Traversable(PolylineShape(Rectangle2D(points(0), parse(proportions,points(0),v)))))
          val inputRequest = InputRequest(7,Some(v),vector2DGuide)

          Start('cad, "create.Input", inputRequest)
        }
        //use second point
        else if (points.length == 1) {

          points = points :+ parse(proportions,points.head,v)

          val p1 =  Some(Vector2D(points.head.x,points.last.y))
          val p2 =  Some(Vector2D(points.last.x,points.head.y))

          //update the image so that it is placed correctly
          if(Siigna.imageBackground._1.isDefined) Siigna.imageBackground = (Some(Siigna.imageBackground._1.get),p1,p2)

          //TODO: send ID of this along to Siigna.imagebackground in order to be able to select it / delete it again
          //Create a rectangle to act as a bound ing box
          Create(PolylineShape(Rectangle2D(p1.get,p2.get)))

          //Siigna.imageBackID = com.siigna.app.Siigna.latestID

          points = List()
          End
        }
      }

      //If End with no point: End module without drawing anything.
      case End :: tail => End
      //get the first point
      case _ => {
        if(Siigna.imageBackground._1.isDefined) {
          Siigna.imageBackground = (None,None,None) //clear background
          End
        }
        else {
          Siigna display ("Choose a background image")
          //get a background image and save (the info is stored in siigna/app(Siigna)
          image = Some(Dialogue.readImage(JPGFileFilter)).get
          Siigna.imageBackground = (image,Some(Vector2D(0,0)),Some(Vector2D(0,0)))

          //track when the image is loaded. Necessary since width and height parameters are unavailable before then.
          tracker.addImage(image.get, 0)
          tracker.waitForID(0)

          val h = Siigna.imageBackground._1.get.getHeight(null)
          val w = Siigna.imageBackground._1.get.getWidth(null)

          proportions = (w.toDouble/h.toDouble)

          //change cursor to crosshair
          Siigna.setCursor(Cursors.crosshair)

          if (points.length == 0) {
            Tooltip.updateTooltip(List("place background image"))
            Start('cad, "create.Input", InputRequest(6,None))
          } else {

            val vector2DGuide = DynamicDrawFromVector2D((v: Vector2D) => Traversable(PolylineShape(Rectangle2D(points(0), (parse(proportions,points(0),v))))))
            val inputRequest = InputRequest(7,Some(points.head),vector2DGuide)
            Start('cad, "create.Input", inputRequest)
          }
        }
      }
    }
  )
}
