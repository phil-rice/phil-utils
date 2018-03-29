package org.validoc.sampleServer

import java.util.ResourceBundle
import java.util.concurrent.Executors

import org.validoc.sample.JsonBundle
import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.utils.cache.{CachingServiceFactory, DurationStaleCacheStategy}
import org.validoc.utils.functions.AsyncForScalaFuture
import org.validoc.utils.http._
import org.validoc.utils.local.ExecutionContextWithLocal
import org.validoc.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.map.NoMapSizeStrategy
import org.validoc.utils.metrics.PrintlnPutMetrics

import scala.concurrent.{ExecutionContext, Future}

class AllNeededThings extends SampleJsonsForCompilation {
  implicit val executors = Executors.newFixedThreadPool(10)
  implicit val exc = new ExecutionContextWithLocal(ExecutionContext.fromExecutor(executors))
  implicit val asyncAndMonad = AsyncForScalaFuture.defaultAsyncForScalaFuture

  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.successful(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new AbstractLogRequestAndResult[Throwable] {
    override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) = messagePostFix + "." + messagePostFix + ":" + strings.mkString(",")
  }
  implicit val cacheFactory = new CachingServiceFactory[Future](DurationStaleCacheStategy(10000000000L, 10000000000000L), NoMapSizeStrategy)
  implicit val failer = Failer.failerForThrowable
  implicit val jsonBundle = JsonBundle()
}
