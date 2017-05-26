package aktorrent.protocol.udp

import java.net.InetSocketAddress
import java.nio.ByteBuffer

import akka.actor.{Actor, ActorLogging, Props, ActorRef}
import akka.io.IO
import akka.io.UdpConnected
import akka.util.ByteString

object UDPClient {

  def props(tracker: InetSocketAddress) =
    Props(new UDPClient(tracker))

}

class UDPClient(tracker: InetSocketAddress) extends Actor with ActorLogging {
  import context.system
  IO(UdpConnected) ! UdpConnected.Connect(self, tracker)

  def receive = {
    case UdpConnected.Connected =>
      context.become(connected(sender()))
  }

  def connected(connection: ActorRef): Receive = {
    case UdpConnected.Received(data) =>
    case msg: String =>
      connection ! UdpConnected.Send(ByteString(msg))
    case UdpConnected.Disconnect =>
      connection ! UdpConnected.Disconnect
    case UdpConnected.Disconnected => context.stop(self)
  }
}
