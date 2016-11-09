package aktorrent

import scala.util.Try

import BencodedType._
import org.parboiled2._
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.syntax.singleton._

final object BencodedType {

  sealed abstract class BType extends Product with Serializable {
    def asBDict: BDict = throw new Exception("Invalid")
  }

  final case class BNumber(value: Long) extends BType {
    override def toString(): String =
      s"i${value}e"
  }

  final case class BString(s: String) extends BType {
    override def toString(): String =
      s"${s.length}:$s"
  }

  final case class BArray(a: Seq[BType]) extends BType
  final case class BDict(fields: Map[String, BType] = Map()) extends BType {
    override def asBDict: BDict = this
    def get(key: String): Option[BType] = fields.get(key)
  }

  case class DeserializationException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
  def deserializationError(msg: String) = throw new DeserializationException(msg)

  trait BFormat[I] {
    def decode(input: BType): I
    // def encode(input: I): BType
  }

  implicit object hNilFormat extends BFormat[HNil] {
    def decode(j: BType) = HNil
    def encode(n: HNil) = BDict()
  }

  implicit object IntBFormat extends BFormat[Int] {
    def decode(value: BType) = value match {
      case BNumber(x) => x.toInt
      case x => deserializationError("Expected Int as BNumber, but got " + x)
    }
    def encode(x: Int) = BNumber(x)
  }

  implicit object LongBFormat extends BFormat[Long] {
    def encode(x: Long) = BNumber(x)
    def decode(value: BType) = value match {
      case BNumber(x) => x
      case x => deserializationError("Expected Long as BNumber, but got " + x)
    }
  }

  implicit object BigIntBFormat extends BFormat[BigInt] {
    def encode(x: BigInt) = {
      require(x ne null)
      BNumber(x.toInt)
    }
    def decode(value: BType) = value match {
      case BNumber(x) => x.toInt
      case BString(x) => BigInt(x)
      case x => deserializationError("Expected BigInt as BNumber, but got " + x)
    }
  }

  implicit object CharBFormat extends BFormat[Char] {
    def encode(x: Char) = BString(String.valueOf(x))
    def decode(value: BType) = value match {
      case BString(x) if x.length == 1 => x.charAt(0)
      case x => deserializationError("Expected Char as single-character BString, but got " + x)
    }
  }

  implicit object StringBFormat extends BFormat[String] {
    def encode(x: String) = {
      require(x ne null)
      BString(x)
    }
    def decode(value: BType) = value match {
      case BString(x) => x
      case x => deserializationError("Expected String as BString, but got " + x)
    }
  }

  implicit def hListFormat[Key <: Symbol, Value, Remaining <: HList](
    implicit
      key: Witness.Aux[Key],
    lazyJfh: Lazy[BFormat[Value]],
    lazyJft: Lazy[BFormat[Remaining]]
  ): BFormat[FieldType[Key, Value] :: Remaining] = new BFormat[FieldType[Key, Value] :: Remaining] {

    val jfh = lazyJfh.value
    val jft = lazyJft.value

    // def encode(hlist: FieldType[Key, Value] :: Remaining) =
    //   jft.encode(hlist.tail).asJsObject :+
    //     (key.value.name -> jfh.encode(hlist.head))

    def decode(value: BType) = {
      val fields = value.asBDict.fields
      val head = jfh.decode(fields(key.value.name))
      val tail = jft.decode(value)
      field[Key](head) :: tail
    }
  }

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
      generic: LabelledGeneric.Aux[A, H],
    repFormat: Lazy[BFormat[H]]
  ): BFormat[A] =
    new BFormat[A] {
      def decode(value: BType) = {
        generic.from(repFormat.value.decode(value))
      }
    }

  // LabelledGeneric[X], value: BType -> HList -> X
  // shapeless.LabelledGeneric[Hola]{type Repr =
  // shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("x")],Int],
  // shapeless.::[Float with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("b")],Float],
  // shapeless.HNil]]}
}

object BFormat {
  def apply[T](implicit f: Lazy[BFormat[T]]): BFormat[T] = f.value
}

case class Hola(a: Long, b: Long)

object X {
  import BFormat._
  val x = BFormat[Hola]
  val zz = x.decode(BDict(Map("a" -> BNumber(1), "b" -> BNumber(3))))
  println(zz)
}

class BEncondingException(s: String) extends RuntimeException

class BencodingParser(val input: ParserInput) extends Parser {
  private var fieldEnd: Int = _

  def InputLine = rule { root ~ EOI }

  def root = rule { zeroOrMore(str | int | list | dict) }

  def str: Rule1[BString] = rule { '0' ~ ':' ~ push(BString("")) | (Number ~ ':' ~> ((length: Long) => run(fieldEnd = cursor + length.toInt)) ~ capture(oneOrMore(test(cursor < fieldEnd) ~ ANY))) ~> (BString(_)) }

  def int: Rule1[BNumber] = rule { 'i' ~ integer ~ 'e' ~> (BNumber(_)) }

  def innerDict = rule { str ~ (str | int | list | dict) ~> ((a: BString, b: BType) => (a, b)) }

  def dict: Rule1[BDict] = rule { 'd' ~ oneOrMore(innerDict) ~ 'e' ~> ((tuples: Seq[Tuple2[BString, BType]]) => BDict(tuples.map(t => (t._1.s, t._2)).toMap)) }

  def integer = rule { ('-' ~ Number ~> (_ * -1)) | Number }

  def list: Rule1[BArray] = rule { 'l' ~ zeroOrMore(str | int | list | dict) ~ 'e' ~> (BArray(_)) }

  def Number = rule { capture(Digits) ~> (_.toLong) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}
