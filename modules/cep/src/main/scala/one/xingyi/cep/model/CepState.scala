package one.xingyi.cep.model

import one.xingyi.cep._
import one.xingyi.core.misc.LongIdMaker

import scala.collection.concurrent.TrieMap


trait CepState {
  def name: String
  def list: List[StatePipeline]
  def findStatePipeline[ED: StringFieldGetter](ed: ED): Option[StatePipeline] = list.find(_.event.accepts(ed))
  def processAtEnd[ED](trieMap: TrieMap[Any, StoredState])(pipelineData: PipelineData)(implicit longIdMaker: LongIdMaker): Option[StoredState]
}

object Terminate extends CepState {
  override def list: List[StatePipeline] = List()
  override def name: String = "terminate"
  override def processAtEnd[ED](trieMap: TrieMap[Any, StoredState])(pipelineData: PipelineData)(implicit longIdMaker: LongIdMaker): Option[StoredState] = {trieMap.remove(pipelineData.key); None}
}

case class UserState(name: String, var list: List[StatePipeline]) extends CepState {
  override def toString: String = s"State($name, pipelines = ${list.mkString(" || ")})"
  override def processAtEnd[ED](trieMap: TrieMap[Any, StoredState])(pipelineData: PipelineData)(implicit longIdMaker: LongIdMaker): Option[StoredState] =
    trieMap.put(pipelineData.key, pipelineData.asStoredStateWithNewState(longIdMaker.getNextId))
}

