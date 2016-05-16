package aktorrent
import java.nio.file.{Files, Paths}

case class MetaInfo(anounce: List[String],
                    info: Option[BDict])

object Test extends BencodeParser {
  def main(args: Array[String]) = {
    val source = scala.io.Source.fromFile("/home/fran/github/aktorrent/l.torrent", "ISO-8859-15")
    println(source.mkString)

    val byteArray = Files.readAllBytes(Paths.get("/home/fran/github/aktorrent/l.torrent"))
    // new Calculator(new ByteReader(byteArray)).InputLine.run() // evaluates to `scala.util.Success(2)`
    parse(new ByteReader(byteArray)).get match {
      case BDict(info) =>
        val a = "announce-list".toCharArray().map(_.toByte).toList
        info.get(BString(a)) match {
          case Some(get) =>
            println(get)
        }
    }
  }
}
