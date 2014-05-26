package org.jetbrains.plugins.scala.components

import beans.BeanProperty

/**
 * Pavel Fatin
 */

class HighlightingSettings {
  @BeanProperty
  var TYPE_AWARE_HIGHLIGHTING_ENABLED: Boolean = false

  @BeanProperty
  var SUGGEST_TYPE_AWARE_HIGHLIGHTING: Boolean = true
}
