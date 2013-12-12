package taf.db

import net.liftweb.mapper._

class Post extends LongKeyedMapper[Post] {
  def getSingleton = Post
  
  def primaryKeyField = id
  object id          extends MappedLongIndex(this)
  object title       extends MappedString(this, 140)
  object contents    extends MappedText(this)
  object published   extends MappedBoolean(this)
}

object Post extends Post with LongKeyedMetaMapper[Post]



