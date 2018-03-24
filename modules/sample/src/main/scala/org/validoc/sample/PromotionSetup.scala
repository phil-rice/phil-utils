package org.validoc.sample

import org.validoc.sample.domain._
import org.validoc.tagless.{ProfileEachEndpointLanguage, TaglessLanguage, TaglessRoot}
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.json.{FromJson, ToJson}

import scala.concurrent.Future
import scala.language.{higherKinds, postfixOps}


trait PromotionServiceNames {
  val mostPopularServiceName = ServiceName("mostPopular")
  val promotionServiceName = ServiceName("promotion")
  val programmeAndProductionServiceName = ServiceName("programmeAndProduction")
}

case class JsonBundle(implicit
                      val toJsonForHomePage: ToJson[HomePage],
                      val toJsonForEnrichedMostPopular: ToJson[EnrichedMostPopular],
                      val fromJsonForMostPopular: FromJson[MostPopular],
                      val fromJsonForPromotion: FromJson[Promotion],
                      val fromJsonForProgramme: FromJson[Programme],
                      val fromJsonForProduction: FromJson[Production])

class PromotionSetup[Wrapper[_, _], M[_], Fail](interpreter: TaglessLanguage[Wrapper, M])(implicit
                                                                                          monadCanFail: MonadCanFail[M, Fail],
                                                                                          failer: Failer[Fail],
                                                                                          jsonBundle: JsonBundle
) extends PromotionServiceNames {

  import interpreter._
  import jsonBundle._
  import org.validoc.utils.endpoint.MatchesServiceRequest._

  val vogue = http(mostPopularServiceName)
  val billboard = http(promotionServiceName)
  val fnord = http(programmeAndProductionServiceName)

  val rawMostPopularService = vogue |+| objectify[MostPopularQuery, MostPopular]
  val rawPromotionService = billboard |+| objectify[PromotionQuery, Promotion]
  val rawProductionService = fnord |+| objectify[ProductionId, Production]
  val rawProgrammeService = fnord |+| objectify[ProgrammeId, Programme]


  val enrichedPromotion = enrich(rawPromotionService).withChild(rawProductionService).mergeInto[EnrichedPromotion]
  val enrichedMostPopular = enrich(rawMostPopularService).withChild(rawProgrammeService).mergeInto[EnrichedMostPopular]

  val homePage = (merge(enrichedPromotion) and enrichedMostPopular into[HomePageQuery, HomePage] ((hpq, ep, emp) => HomePage(emp, ep)))

  val mostPopularEndPoint = enrichedMostPopular |+| logging("homepage") |+| endpoint[MostPopularQuery, EnrichedMostPopular]("/mostpopular", fixedPath(Get))
  val homePageEndPoint = homePage |+| logging("homepage") |+| endpoint[HomePageQuery, HomePage]("/", fixedPath(Get))
  val microservice = chain(mostPopularEndPoint, homePageEndPoint)

}

object PromotionSetup extends App with SampleJsonsForCompilation {
  val root = new TaglessRoot[Future] {}
  import root._

  implicit val language = new root.ForToString

  implicit val jsonBundle: JsonBundle = JsonBundle()

  import org.validoc.utils.functions.AsyncForScalaFuture._
  import ImplicitsForTest._

  val setup = new PromotionSetup[StringHolder, Future, Throwable](new ProfileEachEndpointLanguage(language))
  println(setup.microservice.invertIndent)
}
