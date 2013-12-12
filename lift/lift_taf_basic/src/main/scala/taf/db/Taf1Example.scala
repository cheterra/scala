package taf.db

import net.liftweb.util._
import net.liftweb.mapper._
import java.sql.{Connection, DriverManager}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.mapper.{DB, DefaultConnectionIdentifier} 


case class TestTaf() {

  def dodb : Unit =
  {
      println ("test taf orga")
      //val myTaf: TafTAdminOrg = TafTAdminOrg.create
      DB.defineConnectionManager(DefaultConnectionIdentifier, tafDBVendor)
      // Calculate the name of a table based on the name of the Mapper [details]
      // Must be set in Boot To get snake_case, use this MapperRules.columnName = (_,name) => StringHelpers.snakify(name) 
      MapperRules.tableName = (_, name) => StringHelpers.snakify(name)
      MapperRules.columnName = (_, name) => StringHelpers.snakify(name)

      println ("query all records")
      val orgas: List[TafTAdminOrg] = TafTAdminOrg.findAll
      println ("the result: " + orgas)
  }

}