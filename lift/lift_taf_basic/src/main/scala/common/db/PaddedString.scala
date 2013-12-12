package common.db

import net.liftweb.mapper._

//  trait Paddable {
//     def pad(s: String) = s
//  }

  // not really required: PaddedString
  abstract class PaddedString[T <: Mapper[T]](towner: T, theMaxLen: Int) extends MappedString[T](towner, theMaxLen)  {
    def pad(s: String): String  = s
  }

  abstract class PaddedChar[T <: Mapper[T]](towner: T, theMaxLen: Int) extends PaddedString[T](towner, theMaxLen)  {
    override def pad(s: String): String  = s.padTo(theMaxLen, ' ')
  }

  // 12 chars long, leading space (left padded), no comma, last two digits are decimals
  abstract class PaddedMoney[T <: Mapper[T]](towner: T, theMaxLen: Int) extends PaddedString[T](towner, theMaxLen)  {
    override def pad(s: String): String  = s.padTo(theMaxLen, ' ')
  }
//  abstract class PaddedLongIndex[T <: Mapper[T]](towner: T) extends MappedLongIndex[T](towner)  with Paddable {
//    override def pad(s: String): String  = s
//    //def pad(s: Long):   Long  = s //String  = String.valueOf(s)
//  }

object Padder {

  def pad(s: String): String  = s
//  def pad(s: String, m: Mapper[T]): String = m match {
//       case mx: [PaddedChar] => s.padTo(mx.theMaxLen, ' ')
 //      case mx: [Mapper[T]]  => s
//  }
}

