package org.metaborg.lang.whilelang

import org.metaborg.lang.whilelang.ast.MCommon.SINT

import scala.language.implicitConversions

/**
  * Implicit conversion from integer in the ast to a Scala Int
  */
package object ast {
  implicit def SINTToInt(sINT: SINT): Int = sINT.string.toInt
}
