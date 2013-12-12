package taf.db

import net.liftweb.mapper._

class PayGrp extends LongKeyedMapper[PayGrp] with ExportableList {
  def getSingleton = PayGrp
  // override def fieldOrder = List(dateOf, description, amount)

  //dbName = from taf_t_admin_org o, taf_t_fee_grp g, taf_t_surcharge s
  def primaryKeyField = feeGrpId
  object feeGrpId           extends MappedLongIndex(this)
  object groupName          extends MappedString(this, 80)
  object validFrom          extends MappedDate(this)
  object validTo            extends MappedDate(this)
  object state              extends MappedString(this, 1)
  object vatId              extends MappedInt(this)
  object commentary         extends MappedString(this, 38)
  object orgId              extends MappedLongIndex(this)
  object orgName            extends PaddedChar(this, 4)

  object surcharge          extends PaddedMoney(this, 12)
  object salesDocumentType  extends PaddedChar(this, 2)
  object formOfPayment      extends PaddedChar(this, 2)

  override def getExportList = List(orgName, groupName, state, vatId, surcharge, salesDocumentType, formOfPayment)
}

object PayGrp extends PayGrp with LongKeyedMetaMapper[PayGrp]



