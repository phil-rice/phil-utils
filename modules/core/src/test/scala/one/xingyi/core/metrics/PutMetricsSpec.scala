package one.xingyi.core.metrics

import java.io.{ByteArrayOutputStream, PrintStream}

import one.xingyi.core.UtilsSpec
import one.xingyi.core.strings.Strings

class PutMetricsSpec extends UtilsSpec {


  "NullPutMetrics" should "do nothing" in {
    // ok this is mostly for code coverage. It at least checks that it doesn't die!
    NullPutMetrics(Map("a" -> CountMetricValue))
  }

  "PrintlnPutMetrics" should "Println" in {
    val (_, s) = Strings.recordPrintln(PrintlnPutMetrics(Map("a" -> CountMetricValue)))
    s.trim shouldBe "a -> CountMetricValue"

  }

}
