package aktorrent

import java.nio.file.{Files, Path}

import btype.BParser

import scala.util.Try
import btype.BType._

case class Torrent(announce: String, info: Info)
case class FileInfo(length: Long, path: List[String])
case class Info(
  name: String,
  `piece length`: Int,
  pieces: String,
  length: Option[Long] = None,
  files: List[FileInfo] = List()
) {
  require(pieces.length % 20 == 0)

  def pieceLength: Int = this.`piece length`

  def piecesHash = pieces.grouped(20).toList
}

object MetaInfoReader {

  def read(path: Path) = {
  // def read(path: Path): Try[Map[String, BType]] = {
    val byteArray = Files.readAllBytes(path)
    val parser = new BParser(byteArray)
    val parserResult = parser.InputLine.run()
    parserResult
  }

}
