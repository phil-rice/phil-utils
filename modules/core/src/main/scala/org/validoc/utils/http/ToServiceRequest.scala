package org.validoc.utils.http

import org.validoc.utils.service.ServerContext


/** Typically used when calling out to another microservice */
trait ToServiceRequest[Req] extends (Req => ServiceRequest)

object ToServiceRequest {
  implicit def toServiceRequestFromServiceContext[HttpReq](implicit serverContext: ServerContext[HttpReq, _]) = serverContext.toServiceRequest
}

/** Typically used when being called by the web framework */
trait FromServiceRequest[Req] extends (ServiceRequest => Req)

object FromServiceRequest {
  implicit def fromServiceRequestFromServiceContext[HttpReq](implicit serverContext: ServerContext[HttpReq, _]) = serverContext.fromServiceRequest
}

case class ServiceRequest(method: Method, uri: Uri, acceptHeader: Option[AcceptHeader] = None, otherHeaders: List[Header] = List(), body: Option[Body] = None)