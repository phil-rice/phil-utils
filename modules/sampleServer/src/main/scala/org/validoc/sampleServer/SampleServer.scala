package org.validoc.sampleServer

import org.validoc.sample.PromotionSetup
import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.simpleServer.{EndpointHandler, SimpleHttpServer}
import org.validoc.tagless.{MakeService, Tagless, TaglessLanguage}
import org.validoc.utils.functions.{AsyncForScalaFuture, MonadCanFail, MonadCanFailWithException}
import org.validoc.utils.http._

import scala.concurrent.Future
import scala.language.higherKinds


object SampleServer extends App with SampleJsonsForCompilation {
//
//  implicit val httpFactory = new HttpFactory[Future, ServiceRequest, ServiceResponse] {
//    override def apply(v1: ServiceName) = { req => Future.successful(ServiceResponse(Status(200), Body(s"response; ${req.body.map(_.s).getOrElse("")}"), ContentType("text/html"))) }
//  }
//  implicit val loggingAdapter = PrintlnLoggingAdapter
//  implicit val resourceBundle = ResourceBundle.getBundle("messages")
//  implicit val putMetrics = PrintlnPutMetrics
//  implicit val logRequestAndResult: LogRequestAndResult[Throwable] = new AbstractLogRequestAndResult[Throwable] {
//    override protected def format(messagePrefix: String, messagePostFix: String)(strings: String*) = messagePostFix + "." + messagePostFix + ":" + strings.mkString(",")
//  }
//  implicit val cacheFactory = new CachingServiceFactory[Future](DurationStaleCacheStategy(10000000000L, 10000000000000L), NoMapSizeStrategy)
//
//  val root = new Tagless[Future, Throwable] {}
//  val interpreter = new root.Kleisli()
//
//  implicit val jsonBundle: JsonBundle = JsonBundle()
//
//  implicit val executors = Executors.newFixedThreadPool(10)
//
//  import org.validoc.utils.http.Failer.failerForThrowable
//
//  //  private val debugLanguage = new DebugEachObjectifyEndpoint(language)
//  val setup = new PromotionSetup[root.K, Future, Throwable](interpreter)
//
//  //  println("Dumping")
//  //  println(debugLanguage.dump)

  println("Sample Server")

  val allNeededThings = new AllNeededThings

  import allNeededThings._


  def makeService: MakeService[Future] = new MakeService[Future] {
    override def apply[Wrapper[_, _]](language: TaglessLanguage[Wrapper, Future]): Wrapper[ServiceRequest, Option[ServiceResponse]] =
      new PromotionSetup[Wrapper, Future, Throwable](language).homePageEndPoint
  }

  val root = new Tagless[Future, Throwable]

  val service = root.withProfileAndHtmlPage(makeService)

  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](service)).start()


//  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](setup.microservice)).start()
}
