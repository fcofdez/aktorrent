package aktorrent

import scala.util.Try

import BencodedType._
import org.parboiled2._
import shapeless._
import shapeless.labelled.FieldType

final object BencodedType {

  sealed abstract class BEncoded extends Product with Serializable
  final case class BNumber(n: Long) extends BEncoded
  final case class BString(s: String) extends BEncoded
  final case class BArray(a: Seq[BEncoded]) extends BEncoded
  final case class BDict(o: Map[String, BEncoded]) extends BEncoded {
    def get(key: String): Option[BEncoded] = o.get(key)
  }

  trait BEncodingDecoder[I <: BEncoded, A] {
    def decode(encoded: I): A
  }

  def createDecoder[I <: BEncoded, A](func: I => A): BEncodingDecoder[I, A] =
    new BEncodingDecoder[I, A] {
      def decode(encoded: I): A = func(encoded)
    }

  implicit val bnumberDecoder: BEncodingDecoder[BNumber, Long] =
    createDecoder(num => num.n)

  implicit val BStringDecoder: BEncodingDecoder[BString, String] =
    createDecoder(str => str.s)

  implicit def hlistDecoder[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hEncoder: Lazy[BEncodingDecoder[K, H]],
    tEncoder: BEncodingDecoder[K, T]
  ): BEncodingDecoder[K, FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
      ???
  }
}

trait BEncodingConverter[I, O] {
  def from(s: I): Try[O]
  def to(t: O): I
}

class BEncondingException(s: String) extends RuntimeException

object BEncodingConverter {
  def apply[I, T](implicit st: Lazy[BEncodingConverter[I, T]]): BEncodingConverter[I, T] = st.value

  implicit def deriveClass[A, I, R](implicit gen: Generic.Aux[A, R], conv: BEncodingConverter[I, R]): BEncodingConverter[I, A] = new BEncodingConverter[I, A] {
    def from(s: I): Try[A] = conv.from(s).map(gen.from)
    def to(a: A): I = conv.to(gen.to(a))
  }
}

class BencodingParser(val input: ParserInput) extends Parser {
  private var fieldEnd: Int = _

  def InputLine = rule { root ~ EOI }

  def root = rule { zeroOrMore(str | int | list | dict) }

  def str: Rule1[BString] = rule { '0' ~ ':' ~ push(BString("")) | (Number ~ ':' ~> ((length: Long) => run(fieldEnd = cursor + length.toInt)) ~ capture(oneOrMore(test(cursor < fieldEnd) ~ ANY))) ~> (BString(_)) }

  def int: Rule1[BNumber] = rule { 'i' ~ integer ~ 'e' ~> (BNumber(_)) }

  def innerDict = rule { str ~ (str | int | list | dict) ~> ((a: BString, b: BEncoded) => (a, b)) }

  def dict: Rule1[BDict] = rule { 'd' ~ oneOrMore(innerDict) ~ 'e' ~> ((tuples: Seq[Tuple2[BString, BEncoded]]) => BDict(tuples.map(t => (t._1.s, t._2)).toMap)) }

  def integer = rule { ('-' ~ Number ~> (_ * -1)) | Number }

  def list: Rule1[BArray] = rule { 'l' ~ zeroOrMore(str | int | list | dict) ~ 'e' ~> (BArray(_)) }

  def Number = rule { capture(Digits) ~> (_.toLong) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

// object BencodingSerializer {
//   import BencodedTypes._

// }
