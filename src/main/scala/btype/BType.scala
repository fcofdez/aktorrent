package aktorrent.btype

object BType {

  sealed abstract class BType extends Product with Serializable {
    def asBDict: BDict = throw new Exception("Invalid")

    def :+(other: BType): BType = throw new Exception("Invalid")
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

  final case object BNone extends BType

  final case class BDict(fields: Map[String, BType] = Map()) extends BType {
    override def asBDict: BDict = this

    override def :+(other: BType): BType = {
      other match {
        case d: BDict =>
          BDict(fields ++ d.fields)
        case _ =>
          this
      }
    }
  }
}
