package aktorrent.btype

import aktorrent.btype.BType._
import org.parboiled2._

import scala.language.higherKinds


class BEncodingException(s: String) extends RuntimeException

class BParser(val input: ParserInput) extends Parser {
  private var fieldEnd: Int = _

  def InputLine = rule {
    root ~ EOI
  }

  def root = rule {
    zeroOrMore(str | int | list | dict)
  }

  def str: Rule1[BString] = rule {
    '0' ~ ':' ~ push(BString("")) | (Number ~ ':' ~> ((length: Long) => run(fieldEnd = cursor + length.toInt)) ~ capture(oneOrMore(test(cursor < fieldEnd) ~ ANY))) ~> (BString(_))
  }

  def int: Rule1[BNumber] = rule {
    'i' ~ integer ~ 'e' ~> (BNumber(_))
  }

  def innerDict = rule {
    str ~ (str | int | list | dict) ~> ((a: BString, b: BType) => (a, b))
  }

  def dict: Rule1[BDict] = rule {
    'd' ~ oneOrMore(innerDict) ~ 'e' ~> ((tuples: Seq[Tuple2[BString, BType]]) => BDict(tuples.map(t => (t._1.s, t._2)).toMap))
  }

  def integer = rule {
    ('-' ~ Number ~> (_ * -1)) | Number
  }

  def list: Rule1[BArray] = rule {
    'l' ~ zeroOrMore(str | int | list | dict) ~ 'e' ~> (BArray(_))
  }

  def Number = rule {
    capture(Digits) ~> (_.toLong)
  }

  def Digits = rule {
    oneOrMore(CharPredicate.Digit)
  }
}
