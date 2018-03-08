package org.validoc.utils.language

import org.validoc.utils.aggregate.{EnrichKleisli, EnrichLanguage, MergeKleisli, MergeLanguage}
import org.validoc.utils.cache.{CacheFactory, CacheKleisli}
import org.validoc.utils.endpoint.{ChainKleisli, EndPoint, EndpointKleisli}
import org.validoc.utils.functions.{Async, LiftFunctionKleisli, Monad, MonadCanFailWithException}
import org.validoc.utils.http._
import org.validoc.utils.logging.{DetailedLogging, LogRequestAndResult, LoggingKleisli}
import org.validoc.utils.metrics.{MetricsKleisli, PutMetrics}
import org.validoc.utils.objectify.ObjectifyKleisli
import org.validoc.utils.profiling.ProfileKleisli
import org.validoc.utils.retry.RetryKleisli
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.reflect.ClassTag

trait MicroserviceComposers[M[_]] {

  implicit class ComposeWrapperPimper[RawReq, RawRes](wrapper: RawReq => M[RawRes]) {
    def |+|[Req, Res](fn: (RawReq => M[RawRes]) => (Req => M[Res])): (Req => M[Res]) = fn(wrapper)
    def |++|[Req, Res](fn: (RawReq => M[RawRes]) => EndPoint[M, Req, Res]): EndPoint[M, Req, Res] = fn(wrapper)
  }

}

trait AndAfterKleisli[M[_]] {
  protected implicit def monad: Monad[M]

  def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Req => M[Mid], fn: Mid => Res2): Req => M[Res2] = {
    req: Req => monad.map(raw(req), fn)
  }
  def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Req => M[Mid], fn: Mid => M[Res2]): Req => M[Res2] = {
    req: Req => monad.flatMap(raw(req), fn)
  }

}

trait MicroserviceBuilder[M[_], Fail] extends ObjectifyKleisli[M, Fail] with HttpKlesili[M] with MetricsKleisli[M, Fail] with LoggingKleisli[M, Fail]
  with ChainKleisli[M, Fail] with EndpointKleisli[M] with RetryKleisli[M, Fail] with ProfileKleisli[M, Fail] with CacheKleisli[M] with LiftFunctionKleisli[M]
  with MergeKleisli[M] with EnrichKleisli[M] with AndAfterKleisli[M] {

  protected implicit def async: Async[M]
  protected implicit def monad: MonadCanFailWithException[M, Fail]
  protected def cacheFactory: CacheFactory[M]
  protected implicit def timeService: NanoTimeService
  protected def logReqAndResult: LogRequestAndResult[Fail]
  protected def failer: Failer[Fail]
  protected def putMetrics: PutMetrics

}

class KleisliMicroserviceBuilder[M[_], Fail] {
  type Klesli[Req, Res] = Req => M[Res]

  case class builder()(implicit protected val async: Async[M],
                       protected val monad: MonadCanFailWithException[M, Fail],
                       protected val timeService: NanoTimeService,
                       protected val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                       protected val cacheFactory: CacheFactory[M],
                       protected val logReqAndResult: LogRequestAndResult[Fail],
                       protected val failer: Failer[Fail],
                       protected val putMetrics: PutMetrics,
                       protected val responseParserFailer: ResponseParserFailer[Fail],
                       protected val detailsLoggingForSR: DetailedLogging[ServiceResponse]

  ) extends MicroserviceBuilder[M, Fail] with MergeLanguage[Klesli] with EnrichLanguage[Klesli] with MicroserviceComposers[M]

}
