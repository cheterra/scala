package taf.db

import net.liftweb.mapper._

class Zombies extends LongKeyedMapper[Zombies] with ExportableList {
  def getSingleton = Zombies
  // override def fieldOrder = List(dateOf, description, amount)

  //dbName = select * from taf_t_fee, taf_t_fee_grp g
  def primaryKeyField = feeGrpId
  object feeGrpId           extends MappedLongIndex(this)
  object groupName          extends MappedString(this, 80)
  object validFrom          extends MappedDate(this)
  object validTo            extends MappedDate(this)
  object state              extends MappedString(this, 1)
  object commentary         extends MappedString(this, 38)
  object orgId              extends MappedLongIndex(this)

  object serviceCode       extends PaddedChar(this, 4)
  object serviceCodeFee    extends PaddedChar(this, 4)
  object variantId         extends PaddedChar(this, 4)
  object feePercMinPrice   extends PaddedMoney(this, 12)
  object feePercMaxPrice   extends PaddedMoney(this, 12)
  object feePercValue      extends PaddedMoney(this, 12)

  override def getExportList = List(feeGrpId, groupName, validFrom, validTo, state, orgId, serviceCode, variantId)
}

object Zombies extends Zombies with LongKeyedMetaMapper[Zombies]



