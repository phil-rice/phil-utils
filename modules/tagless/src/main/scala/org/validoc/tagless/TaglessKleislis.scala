package org.validoc.tagless

import org.validoc.utils.cache._
import org.validoc.utils.endpoint.EndPoint
import org.validoc.utils.functions.{Async, MonadCanFailWithException}
import org.validoc.utils.http._
import org.validoc.utils.language.MicroserviceBuilder
import org.validoc.utils.logging._
import org.validoc.utils.metrics.PutMetrics
import org.validoc.utils.time.NanoTimeService

import scala.language.{higherKinds, implicitConversions}
import org.validoc.utils.language.Language._

trait TaglessKleislis[M[_], Fail] extends TaglessRoot[M]{


  case class Kleisli(implicit
                     protected val async: Async[M],
                     protected val monad: MonadCanFailWithException[M, Fail],
                     protected val httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse],
                     protected val logReqAndResult: LogRequestAndResult[Fail],
                     protected val timeService: NanoTimeService,
                     protected val putMetrics: PutMetrics,
                     protected val cacheFactory: CacheFactory[M],
                     protected val failer: Failer[Fail],
                     protected val responseParserFailer: ResponseParserFailer[Fail],
                     protected val detailsLoggingForSR: DetailedLogging[ServiceResponse]) extends
    TaglessLanguage[K, M] with MicroserviceBuilder[M, Fail] {
    override def extraEndpoints(wrapper: K[ServiceResponse, Option[ServiceResponse]]): K[ServiceRequest, Option[ServiceResponse]] = {
      serviceRequest => Option.empty[ServiceResponse].liftM
    }
  }

}
