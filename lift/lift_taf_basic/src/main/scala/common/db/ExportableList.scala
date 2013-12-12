package common.db

import net.liftweb.mapper._

trait ExportableList {
  def getExportList: List[BaseMappedField]
}
