package unfiltered.eventsource.request

/** Match extractor for the `Last-Event-ID` event source request header */
object LastEventId extends unfiltered.request.IntHeader("Last-Event-ID")
