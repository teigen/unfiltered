import unfiltered.undertow.cycle.Plan


class App extends Plan {
  import unfiltered.request._
  import unfiltered.response._
  import unfiltered.directives._, Directives._

  def intent = Directive.Intent.Path {
    case "/a" =>
      for {
        u <- uri
        m <- method
      } yield Ok ~> ResponseString(m + " :: " + u)

    case "/b" =>
      for {
        _ <- POST
        b <- data.as.Int.fail((k, v) => BadRequest ~> ResponseString(v + " is not a valid int for " + k)) ~> data.Requiring[Int].fail(k => BadRequest ~> ResponseString("expected " + k)) named "b"
      } yield Ok ~> ResponseString(""+b)
  }
}

object Server {
  import _root_.io.undertow.Undertow

  def main(args:Array[String]){
    val undertow = Undertow.builder()
      .addListener(8080, "localhost")
      .setHandler(new App)
      .build

    undertow.start()

    new Thread{
      override def run() {
        Console.readLine()
        undertow.stop()
      }
    }.start()
  }
}

