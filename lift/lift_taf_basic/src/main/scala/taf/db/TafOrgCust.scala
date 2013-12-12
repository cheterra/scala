package taf.db

import net.liftweb.mapper._

class TafTOrgCust extends LongKeyedMapper[TafTOrgCust] with ExportableList {
  def getSingleton = TafTOrgCust
  //override def dbDefaultConnectionIdentifier = tafDB

  // override def fieldOrder = List(dateOf, description, amount)

  //dbName = "TAF_T_ORG_CUST"
  def primaryKeyField = orgCustId
  object orgCustId         extends MappedLongIndex(this)
  object combType          extends PaddedString(this, 1)
  object agencyNo          extends PaddedChar(this, 6)
  object businessUnitNo    extends PaddedChar(this, 3)
  object chainId           extends PaddedChar(this, 6)
  object coopId            extends PaddedChar(this, 6)
  object custNo            extends PaddedString(this, 10)
  object companyId         extends PaddedChar(this, 6)
  object corporateId       extends PaddedChar(this, 4)
//  object feeGrpId          extends PaddedLongIndex(this)
  object feeGrpId          extends MappedLongIndex(this)
  object tafnocc           extends MappedInt(this)
  object notaf             extends MappedInt(this)
  object deleted           extends MappedInt(this)
  object nopf              extends MappedInt(this)
  object custType          extends PaddedChar(this, 1)

  // Mapped stuff is in PaddedString.scala

  override def getExportList = List(orgCustId, combType, agencyNo, businessUnitNo, chainId, coopId, companyId, corporateId, feeGrpId, custType)

//  def testPadder(s: String) {
//    feeGrpId.pad(s)
//  }

}

object TafTOrgCust extends TafTOrgCust with LongKeyedMetaMapper[TafTOrgCust]



