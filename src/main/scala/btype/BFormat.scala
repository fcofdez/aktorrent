package aktorrent.btype

import aktorrent.btype.BType._
import shapeless.labelled._
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom

object BFormat {
  def apply[T](implicit f: Lazy[BFormat[T]]): BFormat[T] = f.value

  implicit def genOptionFormat[Z[_], X](implicit
                                        evidence: Z[X] <:< Option[X],
                                        ef: BFormat[X]): BFormat[Option[X]] = new BFormat[Option[X]] {
    def decode(value: BType) = {
      Some(ef.decode(value))
    }

    def encode(x: Option[X]) = {
      x.map(ef.encode).getOrElse(BNone)
    }
  }

  implicit def genTraversableFormat[T[_], E](
                                              implicit
                                              evidence: T[E] <:< GenTraversable[E], // both of kind *->*
                                              cbf: CanBuildFrom[T[E], E, T[E]],
                                              ef: BFormat[E]
                                            ): BFormat[T[E]] = new BFormat[T[E]] {
    def decode(value: BType) = {
      value match {
        case BArray(ar) =>
          val col = ar.map(ef.decode)
          val builder = cbf()
          builder.++=(col)
          builder.result()
        case x => deserializationError("Expected GenTraversable[E] as BArray, but got " + x)
      }
    }

    def encode(x: T[E]) = {
      BArray(x.map(ef.encode).toList)
    }
  }

  def deserializationError(msg: String) = throw new DeserializationException(msg)

  implicit def hListFormat[Key <: Symbol, Value, Remaining <: HList](
                                                                      implicit
                                                                      key: Witness.Aux[Key],
                                                                      lazyJfh: Lazy[BFormat[Value]],
                                                                      lazyJft: Lazy[BFormat[Remaining]]
                                                                    ): BFormat[FieldType[Key, Value] :: Remaining] = new BFormat[FieldType[Key, Value] :: Remaining] {

    val jfh = lazyJfh.value
    val jft = lazyJft.value

    def encode(hlist: FieldType[Key, Value] :: Remaining) =
      jft.encode(hlist.tail).asBDict :+
        BDict(Map(key.value.name -> jfh.encode(hlist.head)))

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

      def encode(v: A) = {
        repFormat.value.encode(generic.to(v))
      }
    }

  trait BFormat[I] {
    def decode(input: BType): I

    def encode(input: I): BType
  }

  case class DeserializationException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)

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
}
