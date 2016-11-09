package aktorrent

import java.nio.file.{Files, Path}

import scala.util.Try

import BencodedType._

case class Torrent(announce: String, info: Info)
case class Info(
  name: String,
  `piece length`: String,
  pieces: String,
  length: Option[Long],
  files: Option[String],
  path: List[String]
)

object MetaInfoReader {

  def read(path: Path) = {
  // def read(path: Path): Try[Map[String, BType]] = {
    val byteArray = Files.readAllBytes(path)
    val parser = new BencodingParser(byteArray)
    val parserResult = parser.InputLine.run()
    parserResult
  }

}
