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

package com.siigna.module.base.file

import java.awt.{Color, Graphics => AWTGraphics, Graphics2D, RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.print.{PageFormat, Paper, Printable, PrinterJob}

import com.siigna._

/**
 * A printing module (currently bitmap-printing only)
 */

//TODO: implement vector-printing
class Print extends Module {

  val margin = Siigna.double("printMargin").getOrElse(13.0)
  val pageFormat = printerJob.defaultPage
  val paper = new Paper()
  val printerJob = PrinterJob.getPrinterJob

  val stateMap: StateMap = Map(
  'Start -> {
      case _ => {
        println("in print")
        try {


          printerJob.setJobName("Siigna Printout");
          printerJob.setCopies(1);

          if (Drawing.boundary.width >= Drawing.boundary.height) {
            // The origin is at the bottom left of the paper with x running bottom to top and y running left to right.
            pageFormat.setOrientation(PageFormat.LANDSCAPE)
          } else {
            // The origin is at the top left of the paper with x running to the right and y running down the paper.
            pageFormat.setOrientation(PageFormat.PORTRAIT)
          }



          paper.setImageableArea(margin, margin, paper.getWidth() - margin * 2, paper.getHeight() - margin * 2);
          pageFormat.setPaper(paper)

          printerJob.setPrintable(new MyPrintable, pageFormat)

          if (printerJob.printDialog() != false) {// choose printer
              printerJob.print()
          }

        } catch {
          case e => println(e)
        }
      }
    }
  )
}

class MyPrintable extends Printable {
  def print(g : AWTGraphics, pageFormat : PageFormat, pageIndex : Int) = {
    if (pageIndex != 0) {
      Printable.NO_SUCH_PAGE
    } else {
      val graphics2D = g.asInstanceOf[Graphics2D]
      val graphics   = new Graphics(graphics2D)

      // Define the boundary for the print
      val boundary     = if (Drawing.boundary.width <= Drawing.boundary.height)
          Rectangle2D(Vector2D(pageFormat.getImageableX,   pageFormat.getImageableY),
                    Vector2D(pageFormat.getImageableWidth, pageFormat.getImageableHeight))
        else
          Rectangle2D(Vector2D(pageFormat.getImageableY, pageFormat.getImageableX),
                    Vector2D(pageFormat.getImageableWidth, pageFormat.getImageableHeight))

      val bufferedImage = new BufferedImage(boundary.width.toInt, boundary.height.toInt, BufferedImage TYPE_INT_RGB)

      // Define the buffer graphics as an instance of <code>Graphics2D</code>
      // (which is much nicer than just <code>Graphicsc</code>).
      val bufferedGraphics2D = bufferedImage.getGraphics.asInstanceOf[Graphics2D]

      // Wraps the graphics-object in our own Graphics-wrapper (more simple API).
      val bufferedGraphics = new Graphics(bufferedGraphics2D)

      // Now clear the view;
      bufferedGraphics2D setBackground("#DDDDDF".color)
      bufferedGraphics2D clearRect(0, 0, boundary.width.toInt, boundary.height.toInt)

      // Draw a white rectangle inside the boundary of the current model.
      bufferedGraphics2D setBackground(Color white)
      bufferedGraphics2D clearRect(boundary.topLeft.x.toInt, boundary.topLeft.y.toInt - boundary.height.toInt, boundary.width.toInt, boundary.height.toInt)

      // Enable anti-aliasing.
      bufferedGraphics2D setRenderingHint(RenderingHints KEY_ANTIALIASING, RenderingHints VALUE_ANTIALIAS_ON)

      // CreateCategory a new transformation-matrix for the model
      // TODO: Get the correct screenToPaperScale
      val screenToPaperScale = 2.845 / Drawing.boundaryScale
      val t1 = TransformationMatrix(-Drawing.boundary.topLeft, 1)
      val t2 = TransformationMatrix(Vector2D(0, 0), 1).flipY.scale(screenToPaperScale)

      // Iterate through the dom and draw the shape on the buffer graphics
      Drawing map( _._2.transform(t1).transform(t2) ) foreach( bufferedGraphics draw)

      // Paint filters and interfaces accesible by the modules.
      // TODO: Test matrixes
      Siigna.paint(bufferedGraphics, t1.concatenate(t2))

      //val outline = PolylineShape.fromRectangle(boundary)

      graphics2D.drawImage(bufferedImage, 0, 0, null)
      Printable.PAGE_EXISTS
    }
  }
}