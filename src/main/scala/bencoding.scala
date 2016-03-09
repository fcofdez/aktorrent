package aktorrent

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.Parsers

trait BencodeConstants {
  final val DEFAULT_STRING_ENCODING = "ISO-8859-15"
  final val DEFAULT_NUMBER_ENCODING = "US-ASCII"

  final val NUMBER_BEGIN: Char = 'i'
  final val NUMBER_END: Char = 'e'

  final val LIST_BEGIN = 'l'
  final val LIST_END = 'e'

  final val DICT_BEGIN = 'd'
  final val DICT_END = 'e'
}

sealed trait BencodeType extends BencodeConstants

case class BString(get: List[Byte]) extends BencodeType {

  def create(enc: String = DEFAULT_STRING_ENCODING) = new String(get.toArray, enc)

  override def toString = {
    //We use a 1-byte encoding since it's compliant with specification
    val output = new String(get.take(200).toArray, DEFAULT_STRING_ENCODING)
    if (get.length > 200) output + " ..." else output
  }
}

case class BInt(get: Int) extends BencodeType {
  override def toString = get.toString
}

case class BList(get: List[BencodeType]) extends BencodeType

case class BDict(get: Map[BString, BencodeType]) extends BencodeType


trait BencodeParser extends Parsers with BencodeConstants {
  override type Elem = Byte

  implicit def char2Parser(ch: Char) = elem(ch.toByte)

  def parse(i: Input) = {
    root(i)
  }

  def delimitedBy[A](left: Parser[Byte], right: Parser[Byte])(p: Parser[A]): Parser[A] = left ~> p <~ right

  def singleDigit = new Parser[String] {

    val re = """\d""".r

    def apply(in: Input) = {
      if (in.atEnd) Failure("End of input reached", in)
      else {
        val n = new String(Array(in.first.asInstanceOf[Byte]), DEFAULT_NUMBER_ENCODING)
        re.findFirstIn(n) map (Success(_, in.rest)) getOrElse(Failure(s"'$n' is not a number", in))
      }
    }
  } named ("digit")

  def natural: Parser[Int] = rep1(singleDigit) ^^ (_.mkString.toInt) named ("natural")

  def signedInt = new Parser[Int] {
    def apply(in: Input) = {
      '-'(in) match {
        case Success(_, rest) => natural(rest) map (_ * -1)
        case _ => natural(in)
      }
    }
  } named ("signedInt")

  def rawByte = new Parser[Byte] {
    def apply(in: Input) = {
      if (in.atEnd) Failure("End of input reached", in)
      else Success(in.first, in.rest)
    }
  } named ("rawBytes")

  def string: Parser[BString] =
    natural ~ ':' >> {
      case size ~ _ => repN(size, rawByte) ^^ (BString(_))
    } named ("string")

  def int: Parser[BInt] = {
    NUMBER_BEGIN~>signedInt<~NUMBER_END ^^ (BInt(_)) named ("int")
  }

  def list: Parser[BList] = {
    delimitedBy(LIST_BEGIN, LIST_END) {
      rep(string | int | list | dict)
    } ^^ (BList(_)) named ("list")
  }

  def dict: Parser[BDict] = {
    delimitedBy(DICT_BEGIN, DICT_END) {
      rep(string ~ (string | int | list | dict))
    } ^^ (_.map { case key ~ value => key -> value }) ^^ (l => BDict(l.toMap)) named ("dict")
  }

  def root = string | int | list | dict
}
