package taf.db

import net.liftweb.mapper._

class TafTFeeGrp extends LongKeyedMapper[TafTFeeGrp] with ExportableList {
  def getSingleton = TafTFeeGrp
  // override def fieldOrder = List(dateOf, description, amount)

  //dbName = "TAF_T_FEE_TABLE"
  def primaryKeyField = feeGrpId
  object feeGrpId           extends MappedLongIndex(this)
  object groupName          extends MappedString(this, 80)
  object validFrom          extends MappedDate(this)
  object validTo            extends MappedDate(this)
  object state              extends MappedString(this, 1)
  object commentary         extends MappedString(this, 38)
  object orgId              extends MappedLongIndex(this)

  override def getExportList = List(feeGrpId, groupName, validFrom, validTo, state, orgId)
}

object TafTFeeGrp extends TafTFeeGrp with LongKeyedMetaMapper[TafTFeeGrp]



