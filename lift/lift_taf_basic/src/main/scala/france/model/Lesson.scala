package france
package model

import scala.xml._
import scala.collection.immutable.Vector


class Lesson (lesson: Node) extends Cat {
  lazy val catList: Seq[Category] = makeCategories
  def getCategories = catList
  protected def makeCategories =  lesson\"Category" filter(!_.isEmpty) map(new Category(_))

  def getAllCategories(cats: Seq[Category] = catList): Seq[Category] =
     cats ++ cats.flatMap(cat => getAllCategories(cat.getCategories))
  //def getCategory(name: String) = getAllCategories(catList).filter(_.name.equals(name))

  override def toString =  "Lesson: " + lesson
}
