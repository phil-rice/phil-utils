package one.xingyi.cep
import one.xingyi.cep.model._


trait LastEventAndData {
  def lastEvent: Event
  def data: Map[Event, StringMap]
  def dataForLastEvent = data(lastEvent)
  def getOrException(name: String) = data(lastEvent)(name)
}

case class LastEventAndDataForAccept(lastEvent: Event, rawMap: StringMap) extends LastEventAndData {val data = Map(lastEvent -> rawMap)}

case class PipelineData(key: Any, ed: Any, startState: CepState, data: Map[Event, StringMap], statePipeline: StatePipeline, lastEvent: Event, emitData: List[EmitData]) extends LastEventAndData {
  def asStoredStateWithNewState(newId: Long) = StoredState(newId, key, statePipeline.finalState(), data)
  override def toString: String =
    s""""PipelineData($key,$ed
       |startState: $startState
       |Data
       |  ${data.mkString("\n  ")}
       |Pipeline: $statePipeline
       |Event $lastEvent
     """.stripMargin

}
object PipelineData {
  //TODO this can be made clearer.
  def makeIfCan[ED: StringFieldGetter](thisEd: ED)(s: StoredState): Option[PipelineData] =
    s.currentState.findStatePipeline(thisEd).map(_.asStartData(thisEd, s))

  def makeFromTimeoutEvent(timeout: TOEvent) = {
    import timeout._
    PipelineData(data.key, Timeout, data.currentState, data.data, statePipeline, timeout, List())
  }

}
