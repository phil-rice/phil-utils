package one.xingyi.cep

import java.text.MessageFormat
import java.util.concurrent.{ScheduledExecutorService, ScheduledFuture, TimeUnit}

import one.xingyi.cep.model._
import one.xingyi.core.language.FunctionLanguage._
import one.xingyi.core.misc.LongIdMaker

import scala.collection.concurrent.TrieMap
import scala.language.experimental.macros
import scala.language.postfixOps


trait StringFieldGetter[ED] extends (StringField => ED => Option[String])

object StringFieldGetter {implicit def stringGetterForCep[ED](implicit cEP: CEP[ED]): StringFieldGetter[ED] = cEP.getString}

trait MessageEmitter extends ((TopicEvent, EmitData) => Unit)

trait CEP[ED] {
  def getString: StringFieldGetter[ED]
}

case class StoredState(id: Long, key: Any, currentState: CepState = Terminate, data: Map[Event, StringMap] = Map())

trait HasKeyBy {def keyby: StringField}

trait TimeoutEventProcessor {
  def startTimeoutCountdown(key: Any, events: List[TOEvent])
}

class SimpleTimeoutEventProcessor(timeoutCepProcessor: TimeoutCepProcessor)(implicit executorService: ScheduledExecutorService) extends TimeoutEventProcessor {
  //the logic around threading doesn't need to be too strict, because it doesn't matter if the scheduled futures actually run: the id story in stored data sorts this out
  //If there timeout has occured and the processTimeout finishes before the next line, then the futures added to the map will override the correct ones.
  val map = new TrieMap[Any, List[ScheduledFuture[_]]]()
  override def startTimeoutCountdown(key: Any, events: List[TOEvent]): Unit = {
    map.get(key).foreach(_.foreach(_.cancel(false)))
    val futures = events.map(e => executorService.schedule(() => timeoutCepProcessor.processTimeout(e), e.duration.toMillis, TimeUnit.MILLISECONDS))
    if (futures.size == 0) map.remove(key) else map.put(key, futures)
  }
}

trait TimeoutCepProcessor {
  def processTimeout:TOEvent => Option[(PipelineData, Option[StoredState])]
}
class CEPProcessor[ED](topicEvent: TopicEvent, preprocess: Processor)(implicit cep: CEP[ED], messageEmitter: MessageEmitter, longIdMaker: LongIdMaker, timeoutEventProcessor: TimeoutEventProcessor) extends TimeoutCepProcessor {
  val map: TrieMap[Any, StoredState] = TrieMap()
  def findLastStateFromString: ED => String => StoredState = ed => key => map.getOrElseUpdate(key, new StoredState(longIdMaker.getNextId, key, preprocess.initial, Map()))
  def findLastStateFromED: ED => Option[StoredState] = cep.getString(preprocess.keyby) ~+?> findLastStateFromString
  def processPipeline: PipelineData => PipelineData = { s => s.statePipeline.execute(s) }
  def processFinalState: PipelineData => (PipelineData, Option[StoredState]) = { s: PipelineData => (s, s.statePipeline.finalState().processAtEnd(map)(s)) }
  def emitMessages: PipelineData => Unit = { s: PipelineData => s.emitData.foreach(messageEmitter(topicEvent, _)) }


  def getStoredStateIfSame: TOEvent => Option[TOEvent] = t => map.get(t.data.key).collect { case s if t.data.id == s.id => t }

  def makeTimeoutEvents: Tuple2[PipelineData, Option[StoredState]] => (Any, List[TOEvent]) = {
    case (pipelineData, Some(storedState)) => (pipelineData.key, storedState.currentState.list.flatMap(p => p.event.makeTimeout(storedState, p)))
    case (pipelineData, _) => (pipelineData.key, List())
  }
  def sendToProcessor: Tuple2[Any, List[TOEvent]] => Unit = {case (key, events) => timeoutEventProcessor.startTimeoutCountdown(key, events)}
  def sortOutTimeouts: Tuple2[PipelineData, Option[StoredState]] => Unit = makeTimeoutEvents ~> sendToProcessor

  def debug[X](s: String) = { x: X => println(MessageFormat.format(s, x.toString)); x }
  def process = findLastStateFromED ~~+?> PipelineData.makeIfCan[ED] ~?> processPipeline ~?^> emitMessages ~?> processFinalState ~?^> sortOutTimeouts

  def processTimeout: TOEvent => Option[(PipelineData, Option[StoredState])] =
    getStoredStateIfSame ~?> PipelineData.makeFromTimeoutEvent ~+?> (_.statePipeline.execute) ~?^> emitMessages ~?> processFinalState ~?^> makeTimeoutEvents
}






