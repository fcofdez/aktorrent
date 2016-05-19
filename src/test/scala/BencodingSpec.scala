package aktorrent

import org.scalatest.WordSpec
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll

case class BencodedString(content: String) {
  def encoded = s"${content.length}:${content}"
}

case class BencodedLong(content: Int) {
  def encoded = s"i${content}e"
}

class BencodedSpec extends WordSpec {

  val genString = for {
    s <- arbitrary[String]
  } yield BencodedString(s)

  implicit val arbS: Arbitrary[BencodedString] = Arbitrary(genString)

  val genLong = for {
    l <- arbitrary[Int]
  } yield BencodedLong(l)

  implicit val arbL: Arbitrary[BencodedLong] = Arbitrary(genLong)

  "A Bencoded parser" when {
    "strings" should {
      val propBencodedStrings = forAll { (str: BencodedString) =>
        val parsedInput = new Bencodingg(str.encoded).InputLine.run().get.asInstanceOf[Vector[String]]
        parsedInput == Vector(str.content)
      }.check
    }

    "longs" should {
      val propBencodedLongs = forAll { (l: BencodedLong) =>
        val parsedInput = new Bencodingg(l.encoded).InputLine.run().get.asInstanceOf[Vector[Int]]
        parsedInput == Vector(l.content)
      }.check
    }
  }
}
