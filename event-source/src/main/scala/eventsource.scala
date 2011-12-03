package unfiltered.eventsource

object Message {
  val EOL = "\n"
  val EOM = "\n\n"
  def toEvent(m: Message) =
    (m.id.map("id: %s" format _) ++ m.name.map("event: %s" format _) ++ m.data.split(EOL).map("data: %s" format _)).
      mkString(EOL)
}

case class Message(data: String, id: Option[Long] = None, name: Option[String] = None)
