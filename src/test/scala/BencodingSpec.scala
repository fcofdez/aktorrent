package aktorrent

import scala.util.{Success, Failure}

import org.parboiled2.ParseError
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll

object BencodeSpec extends Properties("Bencoded") {

  import BencodedTypes._

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
    val parsedInput = new BencodingParser(str.serialize).InputLine.run().get.asInstanceOf[Vector[String]]
    parsedInput == Vector(str.content)
  }

  property("dictProperties") = forAll { (l: List[(BencodedString, BencodedType)]) =>
    val parser = new BencodingParser(serialize(l.toMap))
    parser.InputLine.run() match {
      case Success(e) =>
        val input = l.map((a: Tuple2[BencodedString, BencodedType]) => (a._1.content, a._2.content)).toMap
        input == e.asInstanceOf[Vector[Any]].head
      case Failure(error) =>
        throw new Exception("Invalid input")
    }
  }

  property("list") = forAll { (l: List[BencodedType]) =>
    val parser = new BencodingParser(serialize(l))
    parser.InputLine.run() match {
      case Success(e) =>
        l.map(_.content) == e.toList.head
      case Failure(error) =>
        throw new Exception("Invalid input")
    }
  }

  property("longs") = forAll { (l: BencodedLong) =>
    val parsedInput = new BencodingParser(l.serialize).InputLine.run().get.asInstanceOf[Vector[Int]]
    parsedInput == Vector(l.content)
  }
}
