package aktorrent.protocol.udp

object ProtocolConductor {
  type Event = Int

  object Event {
    def None = 0
    def Completed = 1
    def Started = 2
    def Stopped = 3
  }

  type Action = Int

  object Action {
    def Connect = 0
    def Announce = 1
    def Scrape = 2
    def Error = 3
  }

  case object Start
  case class Connect(transactionId: Int)

  case class Announce(connectionId: Long,
                      transationId: Int,
                      infoHash: String,
                      peerId: String,
                      downloaded: Long,
                      left: Long,
                      uploaded: Long,
                      event: Event,
                      ip: Int,
                      key: Int,
                      port: Short = 0,
                      numWant: Int = -1)

  case class Scrape(connectionId: Long,
                    transactionId: Int,
                    infoHash: List[String])

  case class ScrapeResponse(transactionId: Int,
                            seeders: List[Int],
                            completed: List[Int],
                            leechers: List[Int])

  case class AnnounceResponse(transactionId: Int,
                              interval: Int,
                              leechers: Int,
                              seeders: Int,
                              ips: List[Int],
                              pors: List[Short])

  case class ErrorResponse(transactionId: Int, message: String)
}
