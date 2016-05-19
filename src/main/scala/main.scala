package aktorrent

import java.nio.file.{Files, Paths}

import scala.util.{Failure, Success}

import org.parboiled2.ParseError

case class MetaInfo(
  anounce: List[String],
  info: Option[BDict]
)

object Test {
  def main(args: Array[String]) = {
    val byteArray = Files.readAllBytes(Paths.get("/home/fran/github/aktorrent/kat.torrent"))
    val parser = new Bencodingg(byteArray)
    val a = parser.InputLine.run()
    a match {
      case Success(x) => println(x)
      case Failure(z) => println(parser.formatError(z.asInstanceOf[ParseError]))
    }
  }
}
