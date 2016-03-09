package aktorrent
import java.nio.file.{Files, Paths}

object Test extends BencodeParser {
  def main(args: Array[String]) = {
    val byteArray = Files.readAllBytes(Paths.get("/home/fran/github/aktorrent/l.torrent"))
    println(parse(new ByteReader(byteArray)))
  }
}
