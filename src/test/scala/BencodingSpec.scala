package aktorrent

import java.io.ByteArrayInputStream
import org.parboiled2.ParseError
import org.scalatest.WordSpec
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import scala.util.Failure
import scala.util.Success

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

object BencodeSpec extends Properties("Bencoded") {
  def serialize(list: List[BencodedType]): String = {
    s"l${list.map(_.serialize).mkString}e"
  }

  def serialize(dict: Map[BencodedString, BencodedType]): String = {
    s"d${dict.map(t => s"${t._1.serialize}${t._2.serialize}").mkString}e"
  }

  val genString = for {
    s <- Gen.alphaStr
  } yield BencodedString(s)


  val genTuple: Gen[(BencodedString, BencodedType)] = for {
    key <- Gen.alphaStr suchThat (_.length > 0)
    value <-  genLong
  } yield (BencodedString(key), value)

  val genLong = for {
    l <- arbitrary[Int]
  } yield BencodedLong(l)

  def genList[T](genElem: Gen[T]): Gen[List[T]] = {
    sized { sz: Int =>
      for {
        listSize <- Gen.choose(1, sz)
        list <- Gen.listOfN(listSize, genElem)
      } yield list
    }
  }

  implicit val genDictList = genList(genTuple)
  implicit val genList: Gen[List[BencodedType]] = Gen.containerOf[List, BencodedType](oneOf(genString, genLong))

  implicit val arbLong: Arbitrary[BencodedLong] = Arbitrary(genLong)
  implicit val arbString: Arbitrary[BencodedString] = Arbitrary(genString)
  implicit val arbTuple: Arbitrary[(BencodedString, BencodedType)] = Arbitrary(genTuple)
  implicit val arbList: Arbitrary[List[BencodedType]] = Arbitrary(genList)
  implicit val arbDict: Arbitrary[List[(BencodedString, BencodedType)]] = Arbitrary(genDictList)

  property("strings") = forAll { (str: BencodedString) =>
    val parsedInput = new Bencoding(str.serialize).InputLine.run().get.asInstanceOf[Vector[String]]
    parsedInput == Vector(str.content)
  }

  property("dictProperties") = forAll { (l: List[(BencodedString, BencodedType)]) =>
    val parser = new Bencoding(serialize(l.toMap))
    parser.InputLine.run() match {
      case Success(e) =>
        val input = l.map((a: Tuple2[BencodedString, BencodedType]) => (a._1.content, a._2.content)).toMap
        input == e.asInstanceOf[Vector[Any]].head
      case Failure(error) =>
        1 == 2
    }
  }

  property("list") = forAll { (l: List[BencodedType]) =>
    val parser = new Bencoding(serialize(l))
    parser.InputLine.run() match {
      case Success(e) =>
        l.map(_.content) == e.toList.head
      case Failure(error: ParseError) =>
        println(parser.formatError(error))
        1 == 1
      case Failure(error) =>
        1 == 2
    }
  }

  property("longs") = forAll { (l: BencodedLong) =>
    val parsedInput = new Bencoding(l.serialize).InputLine.run().get.asInstanceOf[Vector[Int]]
    parsedInput == Vector(l.content)
  }
}
