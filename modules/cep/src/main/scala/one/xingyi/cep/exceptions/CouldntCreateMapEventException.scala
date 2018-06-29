package one.xingyi.cep.exceptions
import one.xingyi.cep.PipelineData
import one.xingyi.cep.model.MapEvent


class CouldntCreateMapEventException(state: PipelineData, event: MapEvent) extends RuntimeException(
  s"""
     |State is $state
     |MapEvent $event
   """.stripMargin)