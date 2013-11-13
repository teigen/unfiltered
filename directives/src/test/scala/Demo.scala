import unfiltered.{jetty, netty, filter, directives}
import filter.Plan
import filter.request.ContextPath
import unfiltered.request._
import unfiltered.response._

trait Demo extends Plan with App {
  jetty.Http(8080).filter(this).run()
}

// curl -v http://localhost:8080/example/x
// 405 - method not allowed

// curl -v -XPOST http://localhost:8080/example/x
// 415 - unsupported media type

// curl -v -XPOST http://localhost:8080/example/x -H "Content-Type:application/json" -d '{ "x" : 5 }'
// 406 - not acceptable

// curl -v -XPOST http://localhost:8080/example/x -H "Content-Type:application/json" -d '{ "x" : 5 }' -H "Accept:application/json"

object Demo0 extends Demo {
  // good code, bad http

  def intent = {
    case req @ POST(Path(Seg(List("example", id)))) & Accepts.Json(RequestContentType("application/json")) =>
      Ok ~> JsonContent ~> ResponseBytes(Body.bytes(req))
  }
}

object Demo1 extends Demo {
  // bad code, good http
  def intent = {
    case req @ Path(Seg(List("example", id))) => req match {
      case POST(_) => req match {
        case RequestContentType("application/json") => req match {
          case Accepts.Json(_) =>
            Ok ~> JsonContent ~> ResponseBytes(Body.bytes(req))
          case _ => NotAcceptable
        }
        case _ => UnsupportedMediaType
      }
      case _ => MethodNotAllowed
    }
  }
}

object Demo2 extends App {
  jetty.Http(8080).filter(new DemoPlan2).run()
}

class DemoPlan2 extends Plan {
  // good code, good http
  import directives.cycle._

  // it's simple to define your own directives
  def contentType(tpe:String) =
    when{ case RequestContentType(`tpe`) => } orElse UnsupportedMediaType

  def intent = Intent.Path {
    case Seg(List("example", id)) =>
      for {
        _ <- POST
        _ <- contentType("application/json")
        _ <- Accepts.Json
        r <- request
      } yield Ok ~> JsonContent ~> ResponseBytes(Body bytes r)
  }
}

object DemoPlan2_1 extends App {
  jetty.Http(8080).filter(new DemoPlan2_1).run()
}

class DemoPlan2_1 extends Plan {
  import directives.cycle._

  // existing types can be decorated ( Eq, Gt and Lt )
  implicit val contentType = Directive.Eq{ (R:RequestContentType.type, value:String) =>
    when{ case R(`value`) => value } orElse UnsupportedMediaType
  }

  def intent = Intent.Path {
    case Seg(List("example", id)) =>
      for {
        _ <- POST
        _ <- RequestContentType === "application/json" // <-- look at the awesome syntax
        _ <- Accepts.Json
        r <- request
      } yield Ok ~> JsonContent ~> ResponseBytes(Body bytes r)
  }
}

object Demo3 extends App {
  import org.eclipse.jetty.server.session.SessionHandler

  val http = jetty.Http(8080).filter(new DemoPlan3)
  http.current.setSessionHandler(new SessionHandler)
  http.run()
}

class DemoPlan3 extends Plan {
  import directives.cycle.{Intent => DIntent, _}
  import javax.servlet.http.HttpServletRequest

  val Intent = DIntent.Mapping[HttpServletRequest, String] {
    case ContextPath(_, path) => path
  }

  case class User(name:String)

  def session = request.underlying[HttpServletRequest].map{ _.getSession }

  def user = session.flatMap{ s =>
    val u = Option(s.getAttribute("user")).map(_.asInstanceOf[User])
    getOrElse(u, Redirect("/login"))
  }

  def intent = Intent {
    case "/" =>
      for {
        _ <- GET
        u <- user
      } yield Html5(<h1>Hi {u.name}</h1>)

    case "/login" =>
      val get = for{ _ <- GET } yield
        Html5(
          <form action={"/login"} method="post">
            <input type="text" name="user"/>
            <input type="submit" value="login"/>
          </form>)

      // curl -v http://localhost:8080/login -XPOST
      object userParam extends Params.Extract("user", Params.first)

      val post = for{
        _    <- POST
        name <- userParam.fail ~> ResponseString("user required")
        s    <- session
      } yield {
        s.setAttribute("user", User(name))
        Redirect("/")
      }

      get | post
  }
}

object Demo4Netty extends App {
  val plan = new netty.async.Plan with DemoPlan4 with netty.ServerErrorResponse
  netty.Http(8080).handler(plan).run()
}

object Demo4Filter extends App {
  val plan = new filter.async.Plan with DemoPlan4
  jetty.Http(8080).filter(plan).run()
}

trait DemoPlan4 {
  import directives.async._
  import dispatch.{as => das, _}, Defaults._
  import util.control.Exception.allCatch

  implicit def require[T] = data.Requiring[T].fail(name => BadRequest ~> view(name + " is missing", None))

  def intent = Intent.Path {
    case "/" =>
      ( GET ^^^ view("", None)
      | POST ~> (for {
          loc <- data.as.Required[String] named "location"
          temp <- success(temperature(loc))
        } yield view(loc, temp)))
  }

  def parseTemp(elem:xml.Elem) =
    for {
      kelvin <- (elem \ "temperature" \ "@value").map(_.text).headOption
      c <- allCatch.opt(kelvin.toDouble - 273.15)
    } yield "%1.1f".format(c)

  def temperature(location:String) =
    Http(url("http://api.openweathermap.org/data/2.5/weather") <<?
      Map("q" -> location, "mode" -> "xml") OK das.xml.Elem).map(parseTemp)

  def view(loc: String, temp: Option[String]) = Html(
    <html>
      <body>
        <form method="POST">
          Location:
          <input value={loc} name="location" />
          <input type="submit" />
        </form>
        { temp.map { t => <p>It's {t}°C in {loc}!</p> }.toSeq }
      </body>
    </html>
  )
}
