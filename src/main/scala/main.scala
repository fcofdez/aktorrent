package aktorrent

import java.nio.file.Paths

object Test {
  def main(args: Array[String]): Unit = {
    MetaInfoReader.read(Paths.get("/home/fran/github/aktorrent/kat.torrent"))
  }
}
