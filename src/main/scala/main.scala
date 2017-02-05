package aktorrent
import btype.BFormat
import java.nio.file.Paths

import scala.util.{Failure, Success}

object Test {
  def main(args: Array[String]): Unit = {
    val x = MetaInfoReader.read(Paths.get("/home/fran/github/aktorrent/kat.torrent"))
    val formatter = BFormat[Torrent]
    x match {
      case Success(torren) =>
        val t = formatter.decode(torren(0))
      case Failure(fail) =>
    }
  }
}
