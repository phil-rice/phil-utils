package one.xingyi.cep

import one.xingyi.cep.model._
import one.xingyi.core.language.FunctionLanguage._
import one.xingyi.core.misc.{IdMaker, LongIdMaker}

import scala.collection.concurrent.TrieMap
import scala.language.experimental.macros
import scala.language.postfixOps


trait StringFieldGetter[ED] extends (StringField => ED => Option[String])

object StringFieldGetter {implicit def stringGetterForCep[ED](implicit cEP: CEP[ED]): StringFieldGetter[ED] = cEP.getString}

trait CEP[ED] {
  def getString: StringFieldGetter[ED]
  def sendMessage(topicEvent: TopicEvent, emitData: EmitData)
}

case class StoredState(id: Long, key: Any, currentState: CepState = Terminate, data: Map[Event, StringMap] = Map())

trait HasKeyBy {def keyby: StringField}

class CEPProcessor[ED](topicEvent: TopicEvent, preprocess: Processor)(implicit cep: CEP[ED], longIdMaker: LongIdMaker) {
  val map: TrieMap[Any, StoredState] = TrieMap()
  def findLastStateFromString: ED => String => StoredState = ed => key => map.getOrElseUpdate(key, new StoredState(longIdMaker.getNextId, key, preprocess.initial, Map()))
  def findLastStateFromED: ED => Option[StoredState] = cep.getString(preprocess.keyby) ~+?> findLastStateFromString
  def processPipeline: PipelineData => PipelineData = { s => s.statePipeline.execute(s) }
  def processFinalState: PipelineData => Option[StoredState] = { s: PipelineData => s.statePipeline.finalState().processAtEnd(map)(s) }
  def emitMessages: PipelineData => Unit = { s: PipelineData => s.emitData.foreach(cep.sendMessage(topicEvent, _)) }

  def process: ED => Option[PipelineData] = findLastStateFromED ~~+?> PipelineData.makeIfCan[ED] ~?> processPipeline ~?^> processFinalState ~?^> emitMessages


  def processTimeout: TimeoutOccuredEvent => Option[PipelineData] = {
    timeout =>
      import timeout._
      map.get(storedState.key).collect {
        case currentState if storedState.id == currentState.id =>
          val startPipelineData = PipelineData(storedState.key, Timeout, storedState.currentState, storedState.data, statePipeline, timeout, List())
          val finalData = statePipeline.execute(startPipelineData)
          val newStateToStore = processFinalState(finalData)
          emitMessages(finalData)
          ???
      }


  }
}




