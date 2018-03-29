package org.validoc.tagless

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.cache.{CachableKey, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.{Monad, MonadWithException}
import org.validoc.utils.http._
import org.validoc.utils.logging.{DetailedLogging, SummaryLogging}
import org.validoc.utils.metrics.ReportData
import org.validoc.utils.profiling.{ProfileAs, TryProfileData}
import org.validoc.utils.reflection.ClassTags._
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}
import org.validoc.utils.strings.IndentAnd

import scala.language.higherKinds
import scala.reflect.ClassTag


trait TaglessRoot[M[_]] {

  type K[Req, Res] = Req => M[Res]
}

trait TaglessForString[M[_]] extends TaglessRoot[M] {
  type StringHolder[Req, Res] = IndentAnd[String]

  class ForToString extends TaglessLanguage[StringHolder, M] {
    def systemToString(block: TaglessLanguage[StringHolder, M] => StringHolder[ServiceRequest, ServiceResponse]): String = {
      block(this).invertIndent.defaultToString("&nbsp;&nbsp;", "<br />")
    }
    def systemHtmlEndpoint[Wrapper[_, __]](endpointPath: String, language: TaglessLanguage[Wrapper, M])(block: TaglessLanguage[StringHolder, M] => StringHolder[ServiceRequest, ServiceResponse])(implicit monad: Monad[M]): Wrapper[ServiceRequest, Option[ServiceResponse]] = {
      val html = systemToString(block)
      val htmlFn = { s: ServiceRequest => ServiceResponse(Status(200), Body(html), ContentType("text/html")) }
      language.endpoint[ServiceRequest, ServiceResponse](endpointPath, MatchesServiceRequest.fixedPath(Get))(language.function[ServiceRequest, ServiceResponse]("html")(htmlFn))
    }
    def systemHtmlEndpoint2[Wrapper[_, __]](endpointPath: String, language: TaglessLanguage[Wrapper, M], stringHolder: StringHolder[ServiceRequest, Option[ServiceResponse]]) (implicit monad:MonadWithException[M])= {
      val html = stringHolder.invertIndent.defaultToString("&nbsp;&nbsp;", "<br />")
      val htmlFn: ServiceRequest => ServiceResponse = { s: ServiceRequest => ServiceResponse(Status(200), Body(html), ContentType("text/html")) }
      language.endpoint[ServiceRequest, ServiceResponse](endpointPath, MatchesServiceRequest.fixedPath(Get))(language.function[ServiceRequest, ServiceResponse]("html")(htmlFn))
    }

    override def extraEndpoints(wrapper: StringHolder[ServiceResponse, Option[ServiceResponse]]): StringHolder[ServiceRequest, Option[ServiceResponse]] =
      throw new RuntimeException(s" Cannot imagine at the moment why I would want to call this method")
    override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res) =
      IndentAnd(0, List()).insertLineAndIndent(s"function-$name")
    override def http(name: ServiceName): StringHolder[ServiceRequest, ServiceResponse] =
      IndentAnd(0, List()).insertLineAndIndent(s"http(${name.name})")
    override def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: StringHolder[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): StringHolder[Req, Res] =
      http.insertLineAndIndent(s"objectify[${nameOf[Req]},${nameOf[Res]}]")
    override def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: StringHolder[Req, Mid], fn: Mid => Res2): StringHolder[Req, Res2] =
      raw.insertLineAndIndent(s"andAfter[${nameOf[Mid]},${nameOf[Res2]}]")
    override def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: StringHolder[Req, Mid], fn: Mid => M[Res2]): StringHolder[Req, Res2] =
      raw.insertLineAndIndent(s"andAfterK[${nameOf[Mid]},${nameOf[Res2]}]")
    override def chain(endpoints: StringHolder[ServiceRequest, ServiceResponse]*) =
      IndentAnd.merge("chain", endpoints: _*)
    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: StringHolder[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): StringHolder[Req, Res] =
      raw.insertLineAndIndent(s"endpoint[${nameOf[Req]},${nameOf[Res]}]($normalisedPath,$matchesServiceRequest)")
    override def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"metrics($prefix)")
    override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"cache($name)")
    override def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: StringHolder[Req, Res]): StringHolder[Req, Res] =
      raw.insertLineAndIndent(s"retry($retryConfig)")
    override def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"profile")
    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"logging(Using $messagePrefix)")
    override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: StringHolder[ReqP, ResP], child: StringHolder[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
      IndentAnd.merge("enrich", parent, child)
    override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): StringHolder[ReqM, ResM] =
      IndentAnd.merge("merge2", firstService, secondService)
    override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): StringHolder[ReqM, ResM] =
      IndentAnd.merge("merge3", firstService, secondService, thirdService)
    override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], fourthService: StringHolder[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): StringHolder[ReqM, ResM] =
      IndentAnd.merge("merge4", firstService, secondService, thirdService, fourthService)
  }

}

class DelegatesTaglessLanguage[Wrapper[_, _], M[_]](interpreter: TaglessLanguage[Wrapper, M]) extends TaglessLanguage[Wrapper, M] {
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) =
    interpreter.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw)
  override def chain(endpoints: Wrapper[ServiceRequest, Option[ServiceResponse]]*) = interpreter.chain(endpoints: _*)
  override def http(name: ServiceName) = interpreter.http(name)
  override def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper[Req, Mid], fn: Mid => Res2): Wrapper[Req, Res2] =
    interpreter.andAfter(raw, fn)
  override def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper[Req, Mid], fn: Mid => M[Res2]): Wrapper[Req, Res2] =
    interpreter.andAfterK(raw, fn)
  override def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): Wrapper[Req, Res] =
    interpreter.objectify(http)
  override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper[Req, Res]) =
    interpreter.logging(messagePrefix)(raw)
  override def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: Wrapper[Req, Res]) =
    interpreter.metrics(prefix)(raw)
  override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper[Req, Res]) =
    interpreter.cache(name)(raw)
  override def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: Wrapper[Req, Res]) =
    interpreter.retry(retryConfig)(raw)
  override def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: Wrapper[Req, Res]) =
    interpreter.profile(profileData)(raw)
  override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
    interpreter.enrichPrim(parent, child)
  override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2) =
    interpreter.merge2Prim(firstService, secondService, merger)
  override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) =
    interpreter.merge3Prim(firstService, secondService, thirdService, merger)
  override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4) =
    interpreter.merge4Prim(firstService, secondService, thirdService, fourthService, merger)
  override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res) =
    interpreter.function(name)(fn)
  override def extraEndpoints(wrapper: Wrapper[ServiceRequest, Option[ServiceResponse]]): Wrapper[ServiceRequest, Option[ServiceResponse]] =
    interpreter.extraEndpoints(wrapper)
}