package one.xingyi.cep.model

import one.xingyi.cep.exceptions.CouldntCreateMapEventException
import one.xingyi.cep.{PipelineData, StringMap}

trait PipelineStage {
  def execute[ED](state: PipelineData): PipelineData
}

trait CepAction extends PipelineStage

case class EmitData(map: StringMap)

object Emit extends CepAction {
  override def execute[ED](state: PipelineData): PipelineData = state.copy(emitData = state.emitData :+ EmitData(state.dataForLastEvent))
  override def toString: String = "emit"
}

object purge extends CepAction {
  override def execute[ED](state: PipelineData): PipelineData = state
  override def toString: String = "purge"
}

case class map(event: MapEvent) extends CepAction {
  def updateWith[ED](pipelineData: PipelineData)(newMap: StringMap): PipelineData = pipelineData.copy(lastEvent = event, data = pipelineData.data + (event -> newMap))
  override def execute[ED](pipelineData: PipelineData): PipelineData = event.findDataForThisEvent(pipelineData).fold(throw new CouldntCreateMapEventException(pipelineData, event))(updateWith(pipelineData))
}
