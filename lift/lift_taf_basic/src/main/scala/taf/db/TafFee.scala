package taf.db

import net.liftweb.mapper._

class TafTFee extends LongKeyedMapper[TafTFee] {
  def getSingleton = TafTFee
  
  //dbName = "TAF_T_FEE"
  def primaryKeyField = feeId
  object feeId             extends MappedLongIndex(this)
  object serviceCode       extends PaddedChar(this, 4)
  object serviceCodeFee    extends PaddedChar(this, 4)
  object variantId         extends PaddedChar(this, 4)
  object feePercMinPrice   extends PaddedMoney(this, 12)
  object feePercMaxPrice   extends PaddedMoney(this, 12)
  object feePercValue      extends PaddedMoney(this, 12)
}

object TafTFee extends TafTFee with LongKeyedMetaMapper[TafTFee]



