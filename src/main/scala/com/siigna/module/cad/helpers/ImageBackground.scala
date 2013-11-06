package com.siigna.module.cad.helpers

import com.siigna._
import app.model.shape.ImageShape
import util.event.End
import java.awt.Toolkit
import reflect.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

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

/**
 * A module to
 * a) import a background image and place it on a drawing
 * b) remove a background image from a drawing
 */

class ImageBackground extends Module {

  var draw = false

  //get the image from HDD
  val image = Toolkit.getDefaultToolkit.createImage("c:/test.jpg")
  //var imageLength = image.getGraphics.toString.length
  def imageShape(m : Vector2D) = ImageShape.fromImage(image, mousePosition-Vector2D(100,100),mousePosition+Vector2D(100,100),2200,2200, Attributes())


  val stateMap: StateMap = Map(

    'Start -> {

      //exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => {
        draw = false
        End
      }

      case _ => {
        //if(imageLength.toString.length > 1) {
        println("image: "+image)
        draw = true
        //}

      }
    })

  override def paint(g : Graphics, t : TransformationMatrix) {
    if(draw) {
      g draw PolylineShape(Rectangle2D(mousePosition-Vector2D(100,100),mousePosition+Vector2D(100,100)))
      g draw imageShape(mousePosition)
    }
  }

}
