package com.siigna.module.cad.edit

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics, TexturePaint, Graphics2D}
import com.siigna.Shape

object Hatches {
  //add hatches here...

  def crossHatch(s : Shape) = {
  val bufferedImage : BufferedImage = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB)
  val g : Graphics2D  = bufferedImage.createGraphics()
  g.setColor(Color.black)
  g.fillRect(0, 0, 5, 5)
  g.setColor(Color.blue)
  g.drawLine(0, 0, 5, 5) // \
  g.drawLine(0, 5, 5, 0) // /

  // paint with the texturing brush

  // paint with the texturing brush
  //Rectangle2D rect = new Rectangle2D.Double(0, 0, 5, 5);
  //g.setPaint(new TexturePaint(bufferedImage, s))
  //g.fill(shape)
  }
}
