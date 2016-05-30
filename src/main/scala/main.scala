package aktorrent

import java.nio.file.Paths

object Test {
  def main(args: Array[String]) = {
    MetaInfoReader.read(Paths.get("/home/fran/github/aktorrent/kat.torrent"))
  }
}
