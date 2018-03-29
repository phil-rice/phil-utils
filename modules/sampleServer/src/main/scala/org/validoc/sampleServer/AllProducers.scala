package org.validoc.sampleServer

import org.validoc.sample._
import org.validoc.simpleServer.{EndpointHandler, SimpleHttpServer}
import org.validoc.tagless._
import org.validoc.utils.functions.AsyncForScalaFuture._
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._

import scala.concurrent.Future
import scala.language.higherKinds

class AllProducers[Wrapper[_, _], M[_], Fail](language: TaglessLanguage[Wrapper, M])(implicit
                                                                                     monadCanFail: MonadCanFail[M, Fail],
                                                                                     failer: Failer[Fail],
                                                                                     jsonBundle: JsonBundle) {
  val vogueSetup = new VogueSetup(language)
  val billboardSetup = new BillboardSetup(language)
  val fnordSetup = new FnordSetup(language)

  val endpoints: Seq[Wrapper[ServiceRequest, Option[ServiceResponse]]] = Seq(
    vogueSetup.mostpopularEndpoint,
    billboardSetup.billboardEndpoint,
    fnordSetup.productionEndpoint,
    fnordSetup.programmeEndpoint)
  val microservice = language.chain(endpoints: _*)

  def allEndpoints(otherEndpoints: Wrapper[ServiceRequest, Option[ServiceResponse]]*): Wrapper[ServiceRequest, Option[ServiceResponse]] =
    if (otherEndpoints.size == 0) language.chain(endpoints: _*) else
      language.chain(language.chain(endpoints: _*), language.chain(otherEndpoints: _*))
}


object AllProducers extends App {

  println("All producers")

  val allNeededThings = new AllNeededThings

  import allNeededThings._

  def makeService: MakeService[Future] = new MakeService[Future] {
    override def apply[Wrapper[_, _]](language: TaglessLanguage[Wrapper, Future]): Wrapper[ServiceRequest, Option[ServiceResponse]] =
      new AllProducers[Wrapper, Future, Throwable](language).microservice
  }

  val root = new Tagless[Future, Throwable]

  val service = root.withProfileAndHtmlPage(makeService)

  new SimpleHttpServer(9000, new EndpointHandler[Future, Throwable](service)).start()
}
