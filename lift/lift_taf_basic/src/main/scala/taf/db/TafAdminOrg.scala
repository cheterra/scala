package taf.db

import net.liftweb.mapper._

class TafTAdminOrg extends LongKeyedMapper[TafTAdminOrg] {
  def getSingleton = TafTAdminOrg
  
  //dbName = "TAF_T_ADMIN_ORG"
  def primaryKeyField = orgId
  object orgId           extends MappedLongIndex(this)
  object orgName         extends MappedString(this, 80)
  object commentary      extends MappedString(this, 256)
}

object TafTAdminOrg extends TafTAdminOrg with LongKeyedMetaMapper[TafTAdminOrg]



