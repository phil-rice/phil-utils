package one.xingyi.cep.model

import one.xingyi.cep.{PipelineData, StoredState, StringFieldGetter, StringMap}

import scala.annotation.tailrec

object StatePipeline {
}
case class StatePipeline(event: StartEvent, pipelineStages: List[PipelineStage], finalState: () => CepState) {
  def startData[ED: StringFieldGetter](ed: ED): StringMap = event.makeMap(ed).getOrElse(Map())

  def asStartData[ED: StringFieldGetter](thisEd: ED, s: StoredState) =
    PipelineData(s.key, thisEd, s.currentState, s.data + (event -> startData(thisEd)), this, event, List())

  def execute[ED](startState: PipelineData): PipelineData = execute(startState, pipelineStages)

  final private def execute[ED](state: PipelineData, pipelineStages: List[PipelineStage]): PipelineData = pipelineStages match {
    case Nil => state
    case stage :: tail => execute(stage.execute(state), tail)
  }

  def >>(pipelineStage: PipelineStage) = StatePipeline(event, pipelineStages :+ pipelineStage, finalState)
  def >>(finalState: => CepState) = StatePipeline(event, pipelineStages, () => finalState)

  private def pipelineStageAsString: String = if (pipelineStages.isEmpty) "" else " => " + pipelineStages.mkString(" => ")
  override def toString: String = s"($event$pipelineStageAsString => ${finalState().name})"
}
