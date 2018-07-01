package one.xingyi.cep

import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId}
import one.xingyi.core.language.FunctionLanguage._
import one.xingyi.core.reflection.Macros

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.language.experimental.macros

case class ListenerRef(any: Any)

trait StringFieldGetter[ED] {
  def getString(stringField: StringField): ED => Option[String]
}
trait CEP[ED] extends StringFieldGetter[ED] {
  def listenTo(fn: ED => Unit): ListenerRef
  def stopListeningTo(ref: ListenerRef)
}


class CEPProcessor[ED](topic: Topic, preprocess: Preprocess)(implicit cep: CEP[ED]) {
  val map: TrieMap[Any, StoredState[ED]] = TrieMap()

  def findLastStateFromString: ED => String => StoredState[ED] = ed => key => map.getOrElseUpdate(key, new StoredState[ED](key, ed, preprocess.initial, Map()))
  def findLastStateFromED = cep.getString(preprocess.keyby) ~+?> findLastStateFromString
  def processPipeline: PipelineData[ED] => StoredState[ED] = { s => s.statePipeline.execute(s); s.asStoredStateWithNewState }
  def putBackInMap: StoredState[ED] => Unit = { s => map.put(s.key, s) }

  def process = findLastStateFromED ~~+?> PipelineData.makeStartIfCan[ED] ~?> processPipeline ~?> putBackInMap
}

trait HasKeyBy {
  def keyby: StringField
}

object StringField {
  implicit object hasIdForStringField extends HasId[StringField, Int] {override def apply(v1: StringField): Int = v1.id}
}


abstract class StringField(implicit val aggregator: Aggregator[StringField]) extends HasAggregator[StringField] {
  def id: Int
  def name: String
  def event: Event
  aggregator(this)
  def :=(fn: ValueFn): StringField = new StringFieldWithValue(event, id, name, fn)
  def :=(value: String): StringField = new StringFieldWithValue(event, id, name, _ => value)
  def :==(value: String): StringField = macro Macros.assignmentImpl
  def value(implicit map: StringMap) = map(name)
  override def toString: String = s"StringField(${event.name}, $id, $name)"
}

case class KeyByStringField( name: String) extends StringField()(Aggregator.nullAggregator[StringField]) {
  override def id: Int = -1
  override def event: Event = NullEvent
}
case class SimpleStringField(event: Event, id: Int, name: String)(implicit aggregator: Aggregator[StringField]) extends StringField {
  override def toString: String = s"StringField($id,$name)"
}

case class StringFieldWithValue(event: Event, id: Int, name: String, value: ValueFn)(implicit aggregator: Aggregator[StringField]) extends StringField


case class Topic(topicName: String, version: String)


abstract class Preprocess(name: String, version: String) extends HasKeyBy {
  protected implicit class PipeLineOps(p: StatePipeline) {
    def or(p2: StatePipeline) = List(p, p2)
    def ||(p2: StatePipeline) = List(p, p2)
  }
  protected implicit class PipeLineListOps(p: List[StatePipeline]) {
    def or(p2: StatePipeline) = p :+ p2
    def ||(p2: StatePipeline) = p :+ p2
  }
  def initial: CepState

  def newState(state: UserState, block: => List[StatePipeline]) = {
    setupList = setupList :+ (state, block)
    state
  }
  protected var setupList: List[(UserState, List[StatePipeline])] = List()
  def initialise = setupList.foreach { case (state, list) => state.list = list }
  protected def state(block: StatePipeline): UserState = macro Macros.statePipelineImpl
  protected def state(block: List[StatePipeline]): UserState = macro Macros.statePipelinesImpl
  override def toString: String = s"Preprocess($name,$version:\n   ${setupList.map { t => t._1 }.mkString("\n   ")}"
}



