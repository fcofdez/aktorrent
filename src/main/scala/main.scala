package aktorrent
import java.nio.file.{Files, Paths}
import scala.util.Success

case class MetaInfo(anounce: List[String],
                    info: Option[BDict])

object Test extends BencodeParser {
  def main(args: Array[String]) = {
    // val source = scala.io.Source.fromFile("/home/fran/github/aktorrent/l.torrent", "ISO-8859-15")
    // println(source.mkString)

    val byteArray = Files.readAllBytes(Paths.get("/home/fran/github/aktorrent/l.torrent"))
    val a = new Bencodingg(byteArray).InputLine.run()
    println(a)
  }
}
