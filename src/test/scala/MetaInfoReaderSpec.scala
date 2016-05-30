package aktorrent

import org.scalatest.WordSpec

import java.nio.file.Paths

class MetaInfoReaderSpec extends WordSpec {
  "MetaInfoReader" when {
    "A torrent file path is used as an argument" should {
      "return a Map[String, Any]" in {
        val torrentPath = getClass.getResource("/example.torrent").getPath()
        val metaInfo = MetaInfoReader.read(Paths.get(torrentPath))
        assert(metaInfo.isSuccess)
      }
    }
  }
}
