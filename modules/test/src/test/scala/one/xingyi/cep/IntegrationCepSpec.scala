package one.xingyi.cep
import one.xingyi.cep.model.EmitData
import one.xingyi.core.UtilsSpec
import org.mockito.Mockito

import scala.language.reflectiveCalls

abstract class AbstractIntegrationCepSpec[ED: CEP] extends UtilsSpec with CepFixture[ED] {
  def makeEd(tuples: (String, String)*): ED

  behavior of "CepFixture"

  val edInitial = makeEd("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1", "junk" -> "someJunk")
  val edTwo = makeEd("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress2", "junk" -> "someJunk")
  val edThree = makeEd("customerId" -> "someValue", "type" -> "C", "ipaddress" -> "someIpAddress3", "junk" -> "someJunk")

  it should "process the initial ED" in {
    setup { (cepProcessor, timeoutProcessor, messageEmitter) =>
      cepProcessor.process(edInitial)
      val Some(StoredState(101, "someValue", pp2.ie1Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualMap shouldBe Map(pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1"))
    }
  }

  it should "not process the initial ED a second time" in {
    setup { (cepProcessor, timeoutProcessor, messageEmitter) =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edInitial) shouldBe None

      val Some(StoredState(101, "someValue", pp2.ie1Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualMap shouldBe Map(pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1"))
    }
  }
  it should "not process the second ED initially" in {
    setup { (cepProcessor, timeoutProcessor, messageEmitter) =>
      cepProcessor.process(edTwo)
      val Some(StoredState(100, "someValue", pp2.initial, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualMap shouldBe Map()
    }
  }

  it should "process the second ED after the first keeping the latest values" in {
    setup { (cepProcessor, timeoutProcessor, messageEmitter) =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edTwo)
      val Some(StoredState(102, "someValue", pp2.ie2Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualMap shouldBe Map(
        pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1"),
        pp2.ie2 -> Map("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress2")
      )
    }
  }
  it should "Process the third ED which has a map in it" in {
    setup { (cepProcessor, timeoutProcessor, messageEmitter) =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edTwo)
      val Some((pipelineData, None)) = cepProcessor.process(edThree)
      //      actualState shouldBe terminate

      val expectedMap = Map("ipaddress" -> "someIpAddress1/someIpAddress2/someIpAddress3", "type" -> "A-B-C", "businessEventSubtype" -> "performance-test-data")
      pipelineData.data shouldBe Map(
        pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1"),
        pp2.ie2 -> Map("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress2"),
        pp2.ie3 -> Map("customerId" -> "someValue", "type" -> "C", "ipaddress" -> "someIpAddress3"),
        pp2.map123 -> expectedMap
      )
      Mockito.verify(messageEmitter).apply(be2, EmitData(expectedMap))
    }
  }

  it should "remove the data from the trip map when terminate occurs, so we get a new session" in {
    setup { (cepProcessor, timeoutProcessor, messageEmitter) =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edTwo)
      cepProcessor.map.get("someValue").isDefined shouldBe true
      cepProcessor.process(edThree)
      cepProcessor.map.get("someValue").isDefined shouldBe false
    }
  }
}

import CEPSpec._
class IntegrationCepSpec extends AbstractIntegrationCepSpec[Map[String, String]] {
  override def makeEd(tuples: (String, String)*): Map[String, String] = tuples.toMap
}
