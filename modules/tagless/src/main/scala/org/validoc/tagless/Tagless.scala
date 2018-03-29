package org.validoc.tagless

import org.validoc.utils.aggregate._
import org.validoc.utils.cache.{CachableKey, CacheFactory, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.{Async, MonadCanFailWithException}
import org.validoc.utils.http._
import org.validoc.utils.logging._
import org.validoc.utils.metrics.{PutMetrics, ReportData}
import org.validoc.utils.profiling.{ProfileAs, TryProfileData}
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}
import org.validoc.utils.time.NanoTimeService

import scala.concurrent.Future
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.language.implicitConversions


trait MakeService[M[_]] {
  def apply[Wrapper[_, _]](language: TaglessLanguage[Wrapper, M]): Wrapper[ServiceRequest, Option[ServiceResponse]]
}


class Tagless[M[_], Fail](implicit
                          async: Async[M],
                          monad: MonadCanFailWithException[M, Fail],
                          httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                          logReqAndResult: LogRequestAndResult[Fail],
                          timeService: NanoTimeService,
                          putMetrics: PutMetrics,
                          cacheFactory: CacheFactory[M],
                          failer: Failer[Fail],
                          responseParserFailer: ResponseParserFailer[Fail],
                          detailsLoggingForSR: DetailedLogging[ServiceResponse]) extends TaglessForString[M] with TaglessKleislis[M, Fail] with TaglessProfiles[M] with TwoTagless[M] {
  def withProfileAndHtmlPage(makeService: MakeService[M]): ServiceRequest => M[Option[ServiceResponse]] = {
    //    def makeMicroservice[Wrapper[_, _]](language: TaglessLanguage[Wrapper, Future]) = makeService(language)
    type W[Req, Res] = (StringHolder[Req, Res], ProfilingWrapper[Req, Res])

    val forString = new ForToString
    val kleisliLanguage = Kleisli()
    val profiledAllLanguage = profileEachEndpoint("/profiles", kleisliLanguage)
    val htmlAndProfileLanguage: TaglessLanguage[W, M] = using(forString, profiledAllLanguage).inParallel
    val (html, raw) = makeService(htmlAndProfileLanguage)
    profiledAllLanguage.chain(
      raw,
      profiledAllLanguage.extraEndpoints(raw),
      forString.systemHtmlEndpoint2("/html", profiledAllLanguage, html))
  }

}

trait TaglessLanguage[Wrapper[_, _], M[_]] extends MergeLanguage[Wrapper] with EnrichLanguage[Wrapper] {
  def as[Wrapper2[_, _]](implicit ev: Wrapper[_, _] <:< Wrapper2[_, _]): TaglessLanguage[Wrapper2, M] = this.asInstanceOf[TaglessLanguage[Wrapper2, M]]


  def http(name: ServiceName): Wrapper[ServiceRequest, ServiceResponse]
  def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res): Wrapper[Req, Res]

  def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): Wrapper[Req, Res]
  def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper[Req, Mid], fn: Mid => Res2): Wrapper[Req, Res2]
  def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper[Req, Mid], fn: Mid => M[Res2]): Wrapper[Req, Res2]
  def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]


  def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])
                                            (implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): Wrapper[ServiceRequest, Option[ServiceResponse]]
  def chain(endpoints: Wrapper[ServiceRequest, Option[ServiceResponse]]*): Wrapper[ServiceRequest, Option[ServiceResponse]]
  def extraEndpoints(wrapper: Wrapper[ServiceRequest, Option[ServiceResponse]]): Wrapper[ServiceRequest, Option[ServiceResponse]]


  implicit class ComposeWrapperPimper[RawReq, RawRes](wrapper: Wrapper[RawReq, RawRes]) {
    def |+|[Req, Res](fn: Wrapper[RawReq, RawRes] => Wrapper[Req, Res]): Wrapper[Req, Res] = fn(wrapper)
  }

  def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE]
  def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper[ReqM, ResM]
  def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]
  def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): Wrapper[ReqM, ResM]
}


class TransformTaglessLanguage[Wrapper[_, _], Wrapper2[_, _], M[_]](transform: WrapperTransformer[Wrapper, Wrapper2, M])(implicit evidence: Wrapper2[_, _] <:< Wrapper[_, _]) extends TaglessLanguage[Wrapper2, M] {
  //TODO I have no idea how to avoid these conversions. It's safe to do because of the evidence provided, but not nice
  implicit def toWrapper[Req, Res](w2: Wrapper2[Req, Res]): Wrapper[Req, Res] = w2.asInstanceOf[Wrapper[Req, Res]]
  implicit def toSeqWrapper[Req, Res](w2: Seq[Wrapper2[Req, Res]]): Seq[Wrapper[Req, Res]] = w2.asInstanceOf[Seq[Wrapper[Req, Res]]]

  override def http(name: ServiceName): Wrapper2[ServiceRequest, ServiceResponse] =
    transform("http", name.name, _.http(name))
  override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res): Wrapper2[Req, Res] =
    transform("function", name, _.function(name)(fn))
  override def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: Wrapper2[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): Wrapper2[Req, Res] =
    transform[Req, Res]("objectify", "", _.objectify[Req, Res](http), http)
  override def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper2[Req, Mid], fn: Mid => Res2): Wrapper2[Req, Res2] =
    transform[Req, Res2]("andAfter", "", _.andAfter[Req, Mid, Res2](raw, fn), raw)
  override def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper2[Req, Mid], fn: Mid => M[Res2]): Wrapper2[Req, Res2] =
    transform[Req, Res2]("andAfterK", "", _.andAfterK[Req, Mid, Res2](raw, fn), raw)
  override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform[Req, Res]("logging", messagePrefix, _.logging[Req, Res](messagePrefix)(raw), raw)
  override def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform("metrics", prefix, _.metrics(prefix)(raw), raw)
  override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform("cache", name, _.cache(name)(raw))
  override def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform("retry", retryConfig.toString, _.retry(retryConfig)(raw), raw)
  override def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform("profile", "", _.profile(profileData)(raw), raw)
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper2[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): Wrapper2[ServiceRequest, Option[ServiceResponse]] =
    transform("endpoint", normalisedPath, _.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw), raw)
  override def chain(endpoints: Wrapper2[ServiceRequest, Option[ServiceResponse]]*): Wrapper2[ServiceRequest, Option[ServiceResponse]] =
    transform("chain", "", _.chain(endpoints: _*), endpoints: _*)
  override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper2[ReqP, ResP], child: Wrapper2[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper2[ReqP, ResE] =
    transform("enrich", "", _.enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent, child), parent, child)
  override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper2[Req1, Res1], secondService: Wrapper2[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper2[ReqM, ResM] =
    transform("merge2", "", _.merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService, secondService, merger), firstService, secondService)

  override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper2[Req1, Res1], secondService: Wrapper2[Req2, Res2], thirdService: Wrapper2[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper2[ReqM, ResM] =
    transform("merge3", "", _.merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService, secondService, thirdService, merger), firstService, secondService, thirdService)
  override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper2[Req1, Res1], secondService: Wrapper2[Req2, Res2], thirdService: Wrapper2[Req3, Res3], fourthService: Wrapper2[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): Wrapper2[ReqM, ResM] =
    transform("merge4", "", _.merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService, secondService, thirdService, fourthService, merger), firstService, secondService, thirdService, fourthService)
  override def extraEndpoints(wrapper: Wrapper2[ServiceRequest, Option[ServiceResponse]]): Wrapper2[ServiceRequest, Option[ServiceResponse]] =
    transform("extraEndpoints", "", _.extraEndpoints(wrapper))

}


trait TwoTagless[M[_]] {

  case class using[Wrapper1[_, _], Wrapper2[_, _]](l1: TaglessLanguage[Wrapper1, M], l2: TaglessLanguage[Wrapper2, M]) {

    type Two[Req, Res] = (Wrapper1[Req, Res], Wrapper2[Req, Res])


    def inParallel: TaglessLanguage[Two, M] = new TaglessLanguage[Two, M] {
      implicit def wrapperToW1[Req, Res](t: Two[Req, Res]) = t._1
      implicit def wrapperToW2[Req, Res](t: Two[Req, Res]) = t._2
      override def http(name: ServiceName): Two[ServiceRequest, ServiceResponse] = (l1.http(name), l2.http(name))
      override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res): Two[Req, Res] = (l1.function(name)(fn), l2.function(name)(fn))
      override def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: Two[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): Two[Req, Res] =
        (l1.objectify(http), l2.objectify(http))
      override def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Two[Req, Mid], fn: Mid => Res2): Two[Req, Res2] =
        (l1.andAfter(raw, fn), l2.andAfter(raw, fn))
      override def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Two[Req, Mid], fn: Mid => M[Res2]): Two[Req, Res2] =
        (l1.andAfterK(raw, fn), l2.andAfterK(raw, fn))
      override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Two[Req, Res]): Two[Req, Res] =
        (l1.logging(messagePrefix)(raw), l2.logging(messagePrefix)(raw))
      override def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: Two[Req, Res]): Two[Req, Res] =
        (l1.metrics(prefix)(raw), l2.metrics(prefix)(raw))
      override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Two[Req, Res]): Two[Req, Res] =
        (l1.cache(name)(raw), l2.cache(name)(raw))
      override def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: Two[Req, Res]): Two[Req, Res] =
        (l1.retry(retryConfig)(raw), l2.retry(retryConfig)(raw))
      override def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: Two[Req, Res]): Two[Req, Res] =
        (l1.profile(profileData)(raw), l2.profile(profileData)(raw))
      override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Two[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): Two[ServiceRequest, Option[ServiceResponse]] =
        (l1.endpoint(normalisedPath, matchesServiceRequest)(raw), l2.endpoint(normalisedPath, matchesServiceRequest)(raw))
      override def chain(endpoints: Two[ServiceRequest, Option[ServiceResponse]]*): Two[ServiceRequest, Option[ServiceResponse]] =
        (l1.chain(endpoints.map(_._1): _*), l2.chain(endpoints.map(_._2): _*))
      override def extraEndpoints(wrapper: Two[ServiceRequest, Option[ServiceResponse]]): Two[ServiceRequest, Option[ServiceResponse]] =
        (l1.extraEndpoints(wrapper), l2.extraEndpoints(wrapper))
      override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Two[ReqP, ResP], child: Two[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Two[ReqP, ResE] =
        (l1.enrichPrim(parent, child), l2.enrichPrim(parent, child))
      override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Two[Req1, Res1], secondService: Two[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Two[ReqM, ResM] =
        (l1.merge2Prim(firstService, secondService, merger), l2.merge2Prim(firstService, secondService, merger))
      override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Two[Req1, Res1], secondService: Two[Req2, Res2], thirdService: Two[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Two[ReqM, ResM] =
        (l1.merge3Prim(firstService, secondService, thirdService, merger), l2.merge3Prim(firstService, secondService, thirdService, merger))
      override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Two[Req1, Res1], secondService: Two[Req2, Res2], thirdService: Two[Req3, Res3], fourthService: Two[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): Two[ReqM, ResM] =
        (l1.merge4Prim(firstService, secondService, thirdService, fourthService, merger), l2.merge4Prim(firstService, secondService, thirdService, fourthService, merger))
    }

  }

}


