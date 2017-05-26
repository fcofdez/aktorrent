package aktorrent
import btype.BFormat
import java.nio.file.Paths

import scala.util.{Failure, Success}

object Test {
  def main(args: Array[String]): Unit = {
    val x = MetaInfoReader.read(Paths.get("/home/fran/github/aktorrent/lo.torrent"))
    val formatter = BFormat[Torrent]
    x match {
      case Success(torren) =>
        val t: Torrent = formatter.decode(torren(0))
        println(t.announce)
        t.info.files.foreach(println)
        println( t.info.files.map(_.length).sum / t.info.pieceLength)
        println( t.info.files.map(_.length).sum % t.info.pieceLength)
        println( t.info.piecesHash.length )
      case Failure(fail) =>
    }
  }
}
