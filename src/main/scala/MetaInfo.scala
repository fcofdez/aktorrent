package aktorrent

import java.nio.file.{Files, Path}

import scala.util.Try

object MetaInfoReader {

  def read(path: Path): Try[Map[String, Any]] = {
    val byteArray = Files.readAllBytes(path)
    val parser = new BencodingParser(byteArray)
    val parserResult = parser.InputLine.run()
    parserResult.map(_(0).asInstanceOf[Map[String, Any]])
  }

}
