package org.validoc.sample.domain

import org.validoc.utils.concurrency.Async
import org.validoc.utils.mock.MockHttpService

import scala.language.higherKinds

trait DomainFixture {
  val programmeId1 = ProgrammeId("1")
  val programmeId2 = ProgrammeId("2")

  val programme1 = Programme(programmeId1, "this is programme1")
  val programme2 = Programme(programmeId2, "this is programme2")

  val mostPopular = MostPopular(Seq(programmeId1, programmeId2))
  val enrichedMostPopular = EnrichedMostPopular(Seq(programme1, programme2))

  private val productionId2 = ProductionId("2")
  val production2 = Production(productionId2, "someProduction")
  val promotion = Promotion("somePromotion", List(productionId2))
  val enrichedPromotion = EnrichedPromotion("somePromotion", Seq(production2))

  val homePage = HomePage(enrichedMostPopular, enrichedPromotion)
}

//class DomainMockServers[M[_] : Async] extends DomainFixture {
//  val mockProgrammeService = MockHttpService[M, ProgrammeId, Programme] ((programmeId1, programme1), (programmeId2, programme2))
//  val mockProductionService = MockHttpService[M, ProductionId, Production]((ProductionId("2"), production2))
//  val mockPromotionService = MockHttpService[M, PromotionQuery, Promotion] ((PromotionQuery, promotion))
//
//  val mockMostPopularService = MockHttpService[M, MostPopularQuery, MostPopular] ((MostPopularQuery, mostPopular))
//
//  val mockProductionOrProgrammeService = MockHttpService(mockProductionService, mockProgrammeService)
//}
//
