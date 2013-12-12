package france
package model

import scala.xml._
import scala.collection.immutable.Vector


class Path (cat: Category, prefix: Path = null) {
  def pref: String = if (prefix == null) return "" else return prefix.toString
  override def toString = pref + cat.name + "##"
}

class PathSolver(path: String) {
  lazy val solved = path.split("[#]{2}")
}