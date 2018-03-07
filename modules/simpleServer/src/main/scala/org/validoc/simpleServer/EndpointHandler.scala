package org.validoc.simpleServer

import com.sun.net.httpserver.{Headers, HttpExchange, HttpHandler}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.{Async, MonadCanFailWithException}
import org.validoc.utils.http._
import org.validoc.utils._

import scala.language.higherKinds
import org.validoc.utils.language.Language._

class EndpointHandler[M[_] : Async, Fail](fn: ServiceRequest => M[ServiceResponse])(implicit monadCanFail: MonadCanFailWithException[M, Fail], failer: Failer[M, Fail]) extends HttpHandler {
  override def handle(httpExchange: HttpExchange) = {
    HttpUtils.process(httpExchange) {
      val method = Method(httpExchange.getRequestMethod.toLowerCase())
      val body = Body(Streams.readAll(httpExchange.getRequestBody))
      val uri = Uri(httpExchange.getRequestURI.toString)
      val headers: Headers = httpExchange.getRequestHeaders;
      val contentType = Option(headers.getFirst("content-type")).map(ContentType(_))
      val serviceRequest = ServiceRequest(method, uri, contentType = contentType, body = Some(body))
      fn(serviceRequest).await()
    }
  }
}
