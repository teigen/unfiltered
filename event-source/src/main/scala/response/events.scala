package unfiltered.eventsource.response

import unfiltered.response.{ ComposeResponse, ResponseString }
import unfiltered.eventsource.Message

case class Events(ms: Seq[Message])
extends ComposeResponse(ResponseString(ms.map(Message.toEvent) mkString(Message.EOM) + Message.EOM) ~> EventStream)

