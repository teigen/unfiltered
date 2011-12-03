package unfiltered.eventsource.request

import unfiltered.request.{ Accept, HttpRequest, IntHeader }

/** Match extractor for the `Last-Event-ID` event source request header */
object LastEventId extends IntHeader("Last-Event-ID")

object EventStream {
  def unapply[T](r: HttpRequest[T]) = r match {
    case Accept(values) =>
      if(values.exists { _.equalsIgnoreCase("text/event-stream") })
        Some(r)
      else None
    case _ => None
  }
}
