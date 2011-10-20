// 2010 (C) Copyright by Siigna, all rights reserved.

package com.siigna.module.endogenous.radialmenu.category

import com.siigna.module.endogenous.radialmenu._
import com.siigna.util.collection.DirectedGraph

/**
 * A trait for categories in a radial menu.
 */
trait MenuCategory extends MenuElement {

  /**
   * The color used for background-filling.
   */
  val color : java.awt.Color

  /**
   * The name of the category.
   */
  def name      : String

  /**
   * The parent of the category if any.
   */
  val parent : Option[MenuCategory]

  def C   : Option[MenuCategory] = Some(Start)

  def E   : Option[MenuElement] = None
  def ENE : Option[MenuElement] = None
  def ESE : Option[MenuElement] = None

  def N   : Option[MenuElement] = None
  def NNE : Option[MenuElement] = None
  def NNW : Option[MenuElement] = None

  def W   : Option[MenuElement] = None
  def WNW : Option[MenuElement] = None
  def WSW : Option[MenuElement] = None

  def S   : Option[MenuElement] = None
  def SSW : Option[MenuElement] = None
  def SSE : Option[MenuElement] = None

  def NE  : Option[MenuElement] = None
  def NW  : Option[MenuElement] = None
  def SW  : Option[MenuElement] = None
  def SE  : Option[MenuElement] = None

  /**
   * The state map dictates where the different events leads. This is extremely handy when the user traverses the
   * Radial Menu.
   */
  lazy val stateMap = {
    var g = DirectedGraph[MenuElement, MenuEvent]()

    // Map every item or category, if they are defined.
    if (C  .isDefined) g += (this -> EventC   -> C.get)
    if (E  .isDefined) g += (this -> EventE   -> E.get)
    if (ENE.isDefined) g += (this -> EventENE -> ENE.get)
    if (NNE.isDefined) g += (this -> EventNNE -> NNE.get)
    if (N  .isDefined) g += (this -> EventN   -> N.get)
    if (NNW.isDefined) g += (this -> EventNNW -> NNW.get)
    if (WNW.isDefined) g += (this -> EventWNW -> WNW.get)
    if (W  .isDefined) g += (this -> EventW   -> W.get)
    if (WSW.isDefined) g += (this -> EventWSW -> WSW.get)
    if (SSW.isDefined) g += (this -> EventSSW -> SSW.get)
    if (S  .isDefined) g += (this -> EventS   -> S.get)
    if (SSE.isDefined) g += (this -> EventSSE -> SSE.get)
    if (ESE.isDefined) g += (this -> EventESE -> ESE.get)

    // Return the graph
    g
  }
}
