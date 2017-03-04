package org.validoc.utils.http

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async


class HttpObjectService[M[_] : Async, HttpReq: FromServiceRequest, Req: ToServiceRequest, HttpRes: ToServiceResponse, Res](name: String,
                                                                                                                           rawClient: Service[M, HttpReq, HttpRes],
                                                                                                                           responseProcessor: ResponseProcessor[Req, Res])
  extends Service[M, Req, Res] {

  import Async._
  import org.validoc.utils.functions.Functions._

  val toRequest = implicitly[ToServiceRequest[Req]]
  val fromServiceRequest = implicitly[FromServiceRequest[HttpReq]]
  val toServiceResponse = implicitly[ToServiceResponse[HttpRes]]

  implicit def requestDetails = RequestDetails[Req](name) _

  def processServiceResponse(req: Req) = { serviceResponse: ServiceResponse =>
    serviceResponse.status match {
      case Status.Ok => responseProcessor.statusOk(serviceResponse)
      case Status.NotFound => responseProcessor.statusNotFound(requestDetails(req), serviceResponse)
      case _ => responseProcessor.statusUnexpected(requestDetails(req), serviceResponse)
    }
  }

  override def apply(req: Req): M[Res] = {
    (toRequest ~> fromServiceRequest ~> rawClient transformAndLift(
      responseProcessor.exception(req),
      toServiceResponse ~> processServiceResponse(req))
      ) (req)
  }

}
