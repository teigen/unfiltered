package unfiltered.eventsource

import org.specs._

object EventSourceSpec extends Specification {
  "An EventSource message" should {
    "serialize a basic event" in {
      Message.toEvent(Message("I am an\nevent")) must_== "data: I am an\ndata: event" 
     }
    "serialize a message with an id" in {
      Message.toEvent(
        Message("I am an\nevent", id = Some(1)
              )) must_== "id: 1\ndata: I am an\ndata: event"
    }
    "serialize a message with event and name" in {
      Message.toEvent(
        Message("I am an\nevent", id = Some(1), name = Some("event-name")
              )) must_== "id: 1\nevent: event-name\ndata: I am an\ndata: event"
    }
  }
}
