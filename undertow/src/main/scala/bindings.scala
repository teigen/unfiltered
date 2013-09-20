package unfiltered.undertow

import unfiltered.request._
import unfiltered.response.{Found, HttpResponse}

import _root_.io.undertow.server.HttpServerExchange
import _root_.io.undertow.util.{Headers, HttpString}

import java.io.{InputStreamReader, BufferedReader}
import java.net.URLDecoder

import collection.JavaConverters._

object HttpConfig {
  val DEFAULT_CHARSET = "UTF-8"
}

class RequestBinding(exchange:HttpServerExchange) extends HttpRequest(exchange){
  /** read-once access to request body input stream */
  lazy val inputStream = exchange.getInputStream

  private def charset = Charset(this).getOrElse(HttpConfig.DEFAULT_CHARSET)

  /** buffered reader for request body's input stream */
  lazy val reader = new BufferedReader(new InputStreamReader(inputStream, charset))

  /** The HTTP protocol version */
  def protocol = exchange.getProtocol.toString

  /** HTTP verb in all caps */
  def method = exchange.getRequestMethod.toString

  /** full HTTP request uri including raw query string [[http://tools.ietf.org/html/rfc2616#section-5.1.2]] */
  def uri = exchange.getRequestURI

  def queryParams = exchange.getQueryParameters.asScala.mapValues(_.asScala.toSeq)

  def postParams = this match {
    case POST(RequestContentType(ct)) if ct.contains("application/x-www-form-urlencoded") =>
      URLParser.urldecode(new String(Body.bytes(this), charset))
    case _ => Map.empty[String,Seq[String]]
  }

  lazy val params = queryParams ++ postParams

  /** GET and POST parameter names */
  def parameterNames = params.keySet.iterator

  /** Sequence of values associated with a parameter. Nil if none */
  def parameterValues(param: String) = params.getOrElse(param, Nil)

  /** Iterator of request header names */
  def headerNames = exchange.getRequestHeaders.getHeaderNames.asScala.map(_.toString).iterator

  /** Iterator of request headers */
  def headers(name: String) = exchange.getRequestHeaders.get(new HttpString(name)).asScala.iterator

  /** true if the request is using tls, false otherwise */
  def isSecure = false

  /** address associated with the source of the request */
  def remoteAddr = null
}

class ResponseBinding(exchange:HttpServerExchange) extends HttpResponse(exchange){

  def status(statusCode: Int) {
    exchange.setResponseCode(statusCode)
  }

  def outputStream = exchange.getOutputStream

  /** Sets a redirect */
  def redirect(url: String) {
    exchange.setResponseCode(Found.code)
    exchange.getResponseHeaders.add(Headers.LOCATION, url)
  }

  /** Adds a header */
  def header(name: String, value: String) {
    exchange.getResponseHeaders.add(HttpString.tryFromString(name), value)
  }
}

// TODO copied from netty/bindings.scala
private [undertow] object URLParser {

  def urldecode(enc: String) : Map[String, Seq[String]] = {
    def decode(raw: String) = URLDecoder.decode(raw, HttpConfig.DEFAULT_CHARSET)
    val pairs = enc.split('&').flatMap {
      _.split('=') match {
        case Array(key, value) => List((decode(key), decode(value)))
        case Array(key) if key != "" => List((decode(key), ""))
        case _ => Nil
      }
    }.reverse
    (Map.empty[String, List[String]].withDefault {_ => Nil } /: pairs) {
      case (m, (k, v)) => m + (k -> (v :: m(k)))
    }
  }

}