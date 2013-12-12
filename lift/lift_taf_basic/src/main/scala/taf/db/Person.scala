package taf.db

import net.liftweb.mapper._

class Person extends LongKeyedMapper[Person] with ExportableList {
  def getSingleton = Person

  //override def dbDefaultConnectionIdentifier = tafDB

  //dbName = "PERSON"
  def primaryKeyField = personno
  object personno          extends MappedLongIndex(this)
  object agencyno          extends MappedString(this, 6)
  object businessunitno    extends MappedString(this, 3)
  object customerno        extends MappedString(this, 10)
  object companycode       extends MappedString(this, 6)
  object corporatecode     extends MappedString(this, 4)

  def getExportList = List(agencyno, businessunitno, customerno, companycode, corporatecode)
}

object Person extends Person with LongKeyedMetaMapper[Person]



