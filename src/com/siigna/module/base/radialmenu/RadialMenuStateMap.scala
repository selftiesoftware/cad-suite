/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.base.radialmenu

import com.siigna.util.collection._

object RadialMenuStateMap extends DirectedGraph(
      'Start           -> 'KeyEscape       -> 'End,
      'Start           -> 'MouseDrag       -> 'InteractionTestDrag,
      'InteractionTest -> 'MouseClick      -> 'InteractionTest,
      'InteractionTest -> 'MouseUp         -> 'InteractionTest,
      'InteractionTest -> 'MouseExit       -> 'End,
      'InteractionTest -> 'KeyEscape       -> 'End
)