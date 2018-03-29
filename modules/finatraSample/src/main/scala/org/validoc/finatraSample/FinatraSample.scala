package org.validoc.finatraSample

import java.util.concurrent.Executors

import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Future, FuturePool}
import org.validoc.finatra._
import org.validoc.sample.domain._
import org.validoc.sample.{JsonBundle, PromotionServiceNames, PromotionSetup}
import org.validoc.tagless.{Tagless, TaglessKleislis}
import org.validoc.utils.cache._
import org.validoc.utils.http._
import org.validoc.utils.logging.{AbstractLogRequestAndResult, LogRequestAndResult, PrintlnLoggingAdapter}
import org.validoc.utils.map.MaxMapSizeStrategy
import org.validoc.utils.metrics.PrintlnPutMetrics
import Failer._
import com.twitter.finatra.http.response.ResponseBuilder

class FinatraPromotionSetup(implicit futurePool: FuturePool) extends Controller with SampleJsonsForCompilation {
  implicit val monad = new AsyncForTwitterFuture
  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
    override def apply(v1: ServiceName) = { req => Future.value(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
  }
  implicit val loggingAdapter = PrintlnLoggingAdapter
  //  implicit val resourceBundle = ResourceBundle.getBundle("messages")
  implicit val putMetrics = PrintlnPutMetrics
  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new AbstractLogRequestAndResult[Throwable] {
    override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) = messagePostFix + "." + messagePostFix + ":" + strings.mkString(",")
  }
  implicit val cacheFactory = new CachingServiceFactory[Future](new DurationStaleCacheStategy(100000000l, 10000000000l), new MaxMapSizeStrategy(1000, 100))

  val root = new Tagless[Future, Throwable] {}
  val language = new root.Kleisli()

  implicit val jsonBundle: JsonBundle = JsonBundle()
  implicit val executors = Executors.newFixedThreadPool(10)

  import org.validoc.utils.http.Failer.failerForThrowable


  //  private val debugLanguage = new DebugEachObjectifyEndpoint(language)
  val setup = new PromotionSetup[root.K, Future, Throwable](language)

  import FinatraImplicits._

  def liftEndpoint(fn: ServiceRequest => Future[Option[ServiceResponse]]): Request => Future[ResponseBuilder#EnrichedResponse] = { request: Request =>
    val serviceRequest = implicitly[ToServiceRequest[Request]] apply (request)
    val result = fn(serviceRequest)
    result.map {
      case Some(serRes) => response.status(serRes.status.code).body(serRes.body.s).contentType(serviceRequest.contentType.map(_.s).getOrElse("text/html"))
      case _ => throw new RuntimeException
    }
  }

  get("/")(liftEndpoint(setup.homePageEndPoint))

}

object FinatraSample extends App with PromotionServiceNames {
  val setup = new FinatraPromotionSetup()(FuturePools.fixedPool("FuturePoolForApp", 20))

  //  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new AggregatedServicesInterpreter)
  //  val sd = setup.homePageService


  new FinatraServer(8080, new PingController, setup).main(args)

}
