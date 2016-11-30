package org.metaborg.lang.whilelang

import org.metaborg.lang.whilelang.ast.MCommon.SINT

import scala.language.implicitConversions

/**
  * Created by jeff on 28/11/16.
  */
package object ast {
  implicit def SINTToInt(sINT: SINT): Int = sINT.string.toInt
  implicit def IntToSINT(int: Int): SINT = SINT(int.toString, null)
}
