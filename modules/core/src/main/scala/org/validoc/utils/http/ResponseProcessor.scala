package org.validoc.utils.http

import org.validoc.utils.functions.{Liftable, MonadCanFail, MonadWithException}
import org.validoc.utils.parser.Parser

import scala.annotation.implicitNotFound
import scala.language.higherKinds
import org.validoc.utils._
import org.validoc.utils.exceptions.{EndpointNotFoundException, NotFoundException, UnexpectedStatusCodeException}

trait ResponseParser[Fail, Req, Res] extends (RequestAndServiceResponse[Req] => Either[Fail, Res])

object ResponseParser {
  implicit def defaultDirtyParser[M[_], Fail, Req, Res](implicit parser: Parser[Res]) = new ResponseParser[Fail, Req, Res] {
    override def apply(req: RequestAndServiceResponse[Req]) = req match {
      case RequestAndServiceResponse(req, sr) => Right(parser(sr.body.s))
    }
  }
}

trait Failer[M[_], Fail] {
  def notFound[Req](req: Req, response: ServiceResponse): M[RequestAndServiceResponse[Req]]
  def unexpected[Req](req: Req, response: ServiceResponse): M[RequestAndServiceResponse[Req]]
  def pathNotFound(serviceRequest: ServiceRequest): M[ServiceResponse]
}

object Failer {


  implicit def failerForThrowable[M[_]](implicit monad: MonadWithException[M]) = new Failer[M, Throwable] {
    override def notFound[Req](req: Req, response: ServiceResponse) = monad.exception(new NotFoundException(req, response))
    override def unexpected[Req](req: Req, response: ServiceResponse) = monad.exception(new UnexpectedStatusCodeException(req, response))
    override def pathNotFound(serviceRequest: ServiceRequest) = monad.exception(new EndpointNotFoundException(serviceRequest))
  }

}

