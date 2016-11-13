package aktorrent.btype

import aktorrent.btype.BType._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck._

import scala.util.{Failure, Success}

object BencodeSpec extends Properties("Bencoded") {

  def serialize(list: List[BType]): String = {
    s"l${list.map(_.toString).mkString}e"
  }

  def serialize(dict: Map[BString, BType]): String = {
    s"d${dict.map(t => s"${t._1.toString}${t._2.toString}").mkString}e"
  }

  val genString = for {
    s <- Gen.alphaStr
  } yield BString(s)


  val genTuple: Gen[(BString, BType)] = for {
    key <- Gen.alphaStr suchThat (_.length > 0)
    value <-  genLong
  } yield (BString(key), value)

  val genLong = for {
    l <- arbitrary[Int]
  } yield BNumber(l)

  def genList[T](genElem: Gen[T]): Gen[List[T]] = {
    Gen.nonEmptyListOf(genElem)
  }

  implicit val genDictList = genList(genTuple)
  implicit val genList: Gen[List[BType]] = Gen.containerOf[List, BType](oneOf(genString, genLong))

  implicit val arbLong: Arbitrary[BNumber] = Arbitrary(genLong)
  implicit val arbString: Arbitrary[BString] = Arbitrary(genString)
  implicit val arbTuple: Arbitrary[(BString, BType)] = Arbitrary(genTuple)
  implicit val arbList: Arbitrary[List[BType]] = Arbitrary(genList)
  implicit val arbDict: Arbitrary[List[(BString, BType)]] = Arbitrary(genDictList)

  property("strings") = forAll { (str: BString) =>
    val parsedInput = new BParser(str.toString).InputLine.run().get.asInstanceOf[Vector[BString]]
    parsedInput == Vector(str)
  }

  property("dictProperties") = forAll { (l: List[(BString, BType)]) =>
    val parser = new BParser(serialize(l.toMap))
    val inputMap = l.map((a: Tuple2[BString, BType]) => (a._1.s, a._2)).toMap
    parser.InputLine.run() match {
      case Success(vector) =>
        vector(0) match { // We expect only 1 dictionary
          case BDict(map) =>
            map == inputMap
          case _ =>
            throw new Exception(s"Invalid input")
        }
      case Failure(error) =>
        throw new Exception(s"Invalid input ${error}")
    }
  }

  property("list") = forAll { (l: List[BType]) =>
    val parser = new BParser(serialize(l))
    parser.InputLine.run() match {
      case Success(e) =>
        e(0) match {
          case BArray(seq) =>
            l == seq
          case _ =>
            throw new Exception("Invalid input")
        }
      case Failure(error) =>
        throw new Exception("Invalid input")
    }
  }

  property("longs") = forAll { (l: BNumber) =>
    val parsedInput = new BParser(l.toString).InputLine.run().get.asInstanceOf[Vector[BNumber]]
    parsedInput == Vector(l)
  }
}
