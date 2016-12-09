package org.metaborg.whilelang.scala

/**
  * Created by jeff on 07/12/16.
  */
package object analysis {
  def Some[A](a: A): Option[A] = scala.Some(a)
}
