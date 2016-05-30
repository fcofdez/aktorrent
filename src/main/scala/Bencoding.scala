package aktorrent

import org.parboiled2._
import scala.language.higherKinds

object BencodedTypes {

  sealed abstract class BencodedType {
    def serialize: String
    def content: Any
  }

  case class BencodedString(content: String) extends BencodedType {
    def serialize = s"${content.length}:$content"
  }

  case class BencodedLong(content: Int) extends BencodedType {
    def serialize = s"i${content}e"
  }

}

class BencodingParser(val input: ParserInput) extends Parser {
  private var fieldEnd: Int = _

  def InputLine = rule { root ~ EOI }

  def root = rule { zeroOrMore(str | int | list | dict) }

  def str: Rule1[String] = rule { '0' ~ ':' ~ push("") | (Number ~ ':' ~> ((length: Long) => run(fieldEnd = cursor + length.toInt)) ~ capture(oneOrMore(test(cursor < fieldEnd) ~ ANY))) }

  def int: Rule1[Long] = rule { 'i' ~ integer ~ 'e' }

  def innerDict = rule { str ~ (str | int | list | dict ) ~> ((a: String, b: Any) => (a, b)) }

  def dict: Rule1[Map[String, Any]] = rule { 'd' ~ oneOrMore(innerDict) ~ 'e' ~> ((tuples: Seq[Tuple2[String, Any]]) => tuples.toMap)}

  def integer = rule {('-' ~ Number ~> (_ * -1)) | Number }

  def list: Rule1[Seq[Any]] = rule {'l' ~ zeroOrMore(str | int | list | dict) ~ 'e'}

  def Number = rule { capture(Digits) ~> (_.toLong) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

object BencodingSerializer {
  import BencodedTypes._

}
