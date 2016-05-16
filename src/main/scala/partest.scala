package aktorrent

import org.parboiled2._

class Bencodingg(val input: ParserInput) extends Parser {
  def InputLine = rule { root ~ EOI }

  def root = rule { zeroOrMore(str | int | list) }

  def str = rule { Number ~ ':' ~> ((length: Int) => length.times(capture(CharPredicate.All)))}

  def int = rule { 'i' ~ integer ~ 'e' }

  def integer = rule {('-' ~ Number ~> (_ * -1)) | Number }

  def list: Rule1[Any] = rule {'l' ~ zeroOrMore(str | int | list) ~ 'e'}

  def Number = rule { capture(Digits) ~> (_.toInt) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}
