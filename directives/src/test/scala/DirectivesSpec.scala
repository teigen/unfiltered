package unfiltered.directives

import org.specs._

import unfiltered.request._
import unfiltered.spec.{jetty, netty}

import java.util.concurrent.atomic.AtomicLong

object DirectiveSpecCycleJetty extends jetty.Planned with CycleSpec
object DirectiveSpecCycleNetty extends netty.Planned with CycleSpec
object DirectiveSpecAsyncJetty extends jetty.Served with AsyncSpec {
  def setup = _.filter(unfiltered.filter.async.Planify(intent))
}
object DirectiveSpecAsyncNetty extends netty.Served with AsyncSpec {
  def setup = _.handler(unfiltered.netty.async.Planify(intent))
}

trait CycleSpec extends DirectivesSpec {
  lazy val global = cycle
}
trait AsyncSpec extends DirectivesSpec {
  lazy val global = async
}

trait DirectivesSpec extends unfiltered.spec.Hosted {
  import unfiltered.response._
  import dispatch._, Defaults._

  val global:Global
  import global._

  // it's simple to define your own directives
  def contentType(tpe:String) =
    when{ case RequestContentType(`tpe`) => } orElse UnsupportedMediaType

  // enables `===` in awesome_json case
  implicit val contentTypeAwesome =
    Directive.Eq { (R:RequestContentType.type, value:String) =>
      when { case R(`value`) => value } orElse UnsupportedMediaType
    }

  case class BadParam(msg: String) extends ResponseJoiner(msg)( messages =>
      BadRequest ~> ResponseString(messages.mkString("\n"))
  )

  implicit val asInt =
    data.as.String ~> data.as.Int.fail((name, i) => BadParam(name + " is not an int: " + i))

  implicit def require[T] = data.Requiring[T].fail(name => BadParam(name + " is missing"))

  val asEven = data.Conditional[Int]( _ % 2 == 0 ).fail(
    (name, i) => BadParam(name + " is not even: " + i)
  )

  case class Prize(num: Long)
  val callers = new AtomicLong()
  val MaxPrizes = 3

  // limited time offers. expect a side effect!
  val asPrize = data.Requiring[Prize]
                  .fail(name => BadParam("%s are out of stock".format(name)))
                  .named("prizes", Some(callers.getAndIncrement()).filter(_ < MaxPrizes).map(Prize(_)))

  def intent[A, B] = Intent.Path {
    case "/affirmation" =>
      pointed.success(ResponseString("this request needs no validation"))

    case "/commit_or" =>
      val a = for {
        _ <- GET
        _ <- commit
        _ <- pointed.failure(BadRequest)
      } yield Ok ~> ResponseString("a")
      val b = for {
        _ <- POST
      } yield Ok ~> ResponseString("b")
      a | b

    case Seg(List("accept_json", id)) =>
      for {
        _ <- POST
        _ <- contentType("application/json")
        _ <- Accepts.Json
        r <- request
      } yield Ok ~> JsonContent ~> ResponseBytes(Body.bytes(r))

    case Seg(List("awesome_json", id)) =>
      for {
        _ <- POST
        _ <- RequestContentType === "application/json" // <-- awesome syntax
        _ <- Accepts.Json
        r <- request
      } yield Ok ~> JsonContent ~> ResponseBytes(Body bytes r)

    case Seg(List("limited_offer")) =>
      for {
        prize <- asPrize
      } yield Ok ~> ResponseString(
        "Congratulations. You won prize %d".format(prize.num + 1)
      )

    case Seg(List("valid_parameters")) =>
      for {
        optInt <- data.as.Option[Int] named "option_int"
        reqInt <- data.as.Required[Int] named "require_int"
        evenInt <- (asEven ~> require) named "even_int"
        _ <- data.as.String ~> data.as.Int named "ignored_explicit_int"
      } yield Ok ~> ResponseString((
        evenInt + optInt.getOrElse(0) + reqInt
      ).toString)

    case Seg(List("independent_parameters")) =>
      for {
        optInt & reqInt & evenInt & _ <-
          (data.as.Option[Int] named "option_int") &
          (data.as.Required[Int] named "require_int") &
          ((asEven ~> require) named "even_int") &
          (data.as.String ~> data.as.Int named "ignored_explicit_int")
      } yield Ok ~> ResponseString((
        optInt.getOrElse(0) + reqInt + evenInt
      ).toString)
  }

  def someJson = """{"a": 1}"""

  def localhost = dispatch.host("127.0.0.1", port)

  "Directives commit" should {
    "respond with expected commited error" in {
      Http(localhost / "commit_or").apply().getStatusCode must_== 400
    }
    "try alternative when failing before commit" in {
      Http((localhost / "commit_or").POST).apply().getStatusCode must_== 200
    }
  }
  "Directives" should {
    "response with a condition that is always true" in {
      Http(localhost / "affirmation" OK as.String).apply() must_==(
        "this request needs no validation"
      )
    }
    "respond with expected response given named value" in {
      def expect(n: Int) = {
        val resp = Http(localhost / "limited_offer" > as.String)
        val expected = if (n < MaxPrizes) "Congratulations. You won prize %d".format(n + 1) else "prizes are out of stock"
        resp() must_== expected
      }
      (0 to MaxPrizes + 10).forall(expect(_))
    }
    "respond with json if accepted" in {
      val resp = Http(localhost / "accept_json" / "123"
        <:< Map("Accept" -> "application/json")
        <:< Map("Content-Type" -> "application/json")
        << someJson OK as.String)
      resp() must_== someJson
    }
    "respond with not acceptable accepts header missing" in {
      val resp = Http(localhost / "accept_json" / "123"
        <:< Map("Content-Type" -> "application/json")
        << someJson)
      resp().getStatusCode must_== 406
    }
    "respond with unsupported media if content-type wrong" in {
      val resp = Http(localhost / "accept_json" / "123"
        <:< Map("Accept" -> "application/json")
        <:< Map("Content-Type" -> "text/plain")
        << someJson)
      resp().getStatusCode must_== 415
    }
    "respond with 404 if not matching" in {
      val resp = Http(localhost / "accept_other" / "123"
        << someJson)
      resp().getStatusCode must_== 404
    }
  }
  "Directives decorated" should {
    "respond with json if accepted" in {
      val resp = Http((localhost / "awesome_json" / "123"
        <:< Map("Accept" -> "application/json")
        <:< Map("Content-Type" -> "application/json"))
        << someJson OK as.String)
      resp() must_== someJson
    }
    "respond with unsupported media if content-type wrong" in {
      val resp = Http(localhost / "awesome_json" / "123"
        <:< Map("Accept" -> "application/json")
        <:< Map("Content-Type" -> "text/plain")
        << someJson)
      resp().getStatusCode must_== 415
    }
    "respond with unsupported media if content-type missing" in {
      val resp = Http(localhost / "awesome_json" / "123"
        <:< Map("Accept" -> "application/json")
        << someJson)
      resp().getStatusCode must_== 415
    }
  }
  "Directive parameters" should {
    "respond with parameter if accepted" in {
      val resp = Http(localhost / "valid_parameters"
        << Map(
          "option_int" -> 3.toString,
          "require_int" -> 4.toString,
          "even_int" -> 8.toString
        ) OK as.String)
      resp() must_== "15"
    }
    "respond if optional parameters are missing" in {
      val resp = Http(localhost / "valid_parameters"
        << Map(
          "require_int" -> 4.toString,
          "even_int" -> 8.toString
        ) OK as.String)
      resp() must_== "12"
    }
    "fail if even format is wrong" in {
      val resp = Http(localhost / "valid_parameters"
        << Map(
          "require_int" -> 4.toString,
          "even_int" -> 7.toString
        ))
      resp().getStatusCode must_== 400
      resp().getResponseBody must_== "even_int is not even: 7"
    }
    "fail if int format is wrong" in {
      val resp = Http(localhost / "valid_parameters"
        << Map(
          "require_int" -> 4.toString,
          "even_int" -> "eight"
        ))
      resp().getStatusCode must_== 400
      resp().getResponseBody must_== "even_int is not an int: eight"
    }
    "fail if required parameter is missing" in {
      val resp = Http(localhost / "valid_parameters"
        << Map(
          "require_int" -> 4.toString
        ))
      resp().getStatusCode must_== 400
      resp().getResponseBody must_== "even_int is missing"
    }
  }
  "Directive independent parameters" should {
    "respond with parameter if accepted" in {
      val resp = Http(localhost / "independent_parameters"
        << Map(
          "option_int" -> 3.toString,
          "require_int" -> 4.toString,
          "even_int" -> 8.toString
        ) OK as.String)
      resp() must_== "15"
    }
    "respond with all errors" in {
      val resp = Http(localhost / "independent_parameters"
        << Map(
          "option_int" -> "four",
          "even_int" -> 7.toString
        ))
      resp().getStatusCode must_== 400
      resp().getResponseBody must_== """option_int is not an int: four
require_int is missing
even_int is not even: 7"""
    }
  }
}
