package unfiltered.undertow
package cycle

import io.undertow.server.{HttpHandler, HttpServerExchange}
import unfiltered.request.HttpRequest

object Plan {
  type Intent = unfiltered.Cycle.Intent[HttpServerExchange, HttpServerExchange]
}

object Intent {
  def apply(intent:Plan.Intent) = intent
}

trait Plan extends HttpHandler {

  def intent:Plan.Intent

  def onPass(request:HttpRequest[HttpServerExchange]){}

  def handleRequest(exchange: HttpServerExchange) {
    // TODO hmmm...
    exchange.startBlocking()
    val request  = new RequestBinding(exchange)
    val response = new ResponseBinding(exchange)
    intent.fold(onPass, (_, rf) => {
      rf.apply(response)
    })(request)

    exchange.endExchange()
  }
}