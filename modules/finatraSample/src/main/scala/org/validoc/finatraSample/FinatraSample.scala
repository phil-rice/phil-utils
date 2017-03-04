package org.validoc.finatraSample

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.Future
import org.validoc.PromotionSetup
import org.validoc.finatra._
import org.validoc.language.{MakeHttpService, ServiceData}
import org.validoc.language.{MakeHttpService, ServiceData, ServiceInterpreters}

object FinatraSample extends App with FinatraAdapter {

  import ServiceInterpreters._

  implicit val makeHttpService = MakeHttpService(MockFinatraService("mostPopular", "promotion", "programmeAndProductions"))

  implicit val futurePool = FuturePools.fixedPool("pool", 20)

  val setup = new PromotionSetup[ServiceData, Future, Request, Response](new ServiceInterpreters.ServicesGroupedForAsync)

  val sd = setup.homePageService
  println(sd)
  new FinatraServer(8080, new PingController, new EndpointController(sd)).main(args)

}
