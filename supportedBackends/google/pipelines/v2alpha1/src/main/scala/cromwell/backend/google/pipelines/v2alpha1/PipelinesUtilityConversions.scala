package cromwell.backend.google.pipelines.v2alpha1

import java.time.OffsetDateTime

import com.google.api.services.genomics.v2alpha1.model.{Accelerator, Disk, Event, Mount}
import cromwell.backend.google.pipelines.common.api.PipelinesApiRequestFactory.CreatePipelineParameters
import cromwell.backend.google.pipelines.common.io.{DiskType, PipelinesApiAttachedDisk}
import cromwell.backend.google.pipelines.common.{GpuResource, PipelinesApiRuntimeAttributes}
import cromwell.backend.google.pipelines.v2alpha1.api.Delocalization._
import cromwell.core.ExecutionEvent
import cromwell.core.logging.JobLogger
import mouse.all._

import scala.language.postfixOps
import scala.util.Try

trait PipelinesUtilityConversions {
  def toAccelerator(gpuResource: GpuResource) = new Accelerator().setCount(gpuResource.gpuCount.value.toLong).setType(gpuResource.gpuType.toString)
  def toMachineType(jobLogger: JobLogger)(attributes: PipelinesApiRuntimeAttributes) = MachineConstraints.machineType(attributes.memory, attributes.cpu, jobLogger)
  def toMounts(parameters: CreatePipelineParameters): List[Mount] = parameters.runtimeAttributes.disks.map(toMount).toList
  def toDisks(parameters: CreatePipelineParameters): List[Disk] = parameters.runtimeAttributes.disks.map(toDisk).toList
  def toMount(disk: PipelinesApiAttachedDisk) = new Mount()
    .setDisk(disk.name)
    .setPath(disk.mountPoint.pathAsString)
  def toDisk(disk: PipelinesApiAttachedDisk) = new Disk()
    .setName(disk.name)
    .setSizeGb(disk.sizeGb)
    .setType(disk.diskType |> toV2DiskType)
  private def shouldPublish(event: Event): Boolean = {
    // The Docker image used for CWL output parsing causes some complications for the timing diagram. Docker image
    // pulling is done automatically by PAPI v2 and therefore does not correspond to any Actions generated in Cromwell.
    // Since there are no Actions there are no labeled Actions and therefore Docker pull events do not get grouped into
    // any labeled categories. For regular prefetched images this is okay since they don't conflict with any other labeled
    // activity and we actually want to see that detail on the timing diagram. But the CWL output parsing image pull happens
    // in the middle of other 'Delocalization' events. Since it is not a 'Delocalization' event it appears to the timing diagram
    // logic to be concurrent to 'Delocalization' and another row is wrongly added to the timing diagram. The logic here checks
    // if the event description matches a CWL Docker image pull and if so suppresses publication, which is enough to
    // make the timing diagram happy.
    !event.getDescription.matches(s"""(Started|Stopped) pulling "$CwlOutputJsonProcessingDockerImage"""")
  }

  def toExecutionEvent(actionIndexToEventType: Map[Int, String])(event: Event): Option[ExecutionEvent] = {
    val groupingFromAction = for {
      rawValue <- Option(event.getDetails.get("actionId"))
      integerValue <- Try(Integer.valueOf(rawValue.toString)).toOption
      group <- actionIndexToEventType.get(integerValue)
    } yield group

    // There are both "Started pulling" and "Stopped pulling" events but these are confusing for metadata, especially on the
    // timing diagram. Create a single "Pulling <docker image>" grouping to absorb these events.
    def groupingFromPull: Option[String] = List("Started", "Stopped") flatMap { k =>
      Option(event.getDescription) collect { case d if d.startsWith(s"$k pulling") => "Pulling" + d.substring(s"$k pulling".length)}
    } headOption

    shouldPublish(event).option(
      ExecutionEvent(
        name = event.getDescription,
        offsetDateTime = OffsetDateTime.parse(event.getTimestamp),
        grouping = groupingFromAction.orElse(groupingFromPull)
      ))
  }

  private def toV2DiskType(diskType: DiskType) = diskType match {
    case DiskType.HDD => "pd-standard"
    case DiskType.SSD => "pd-ssd"
    case DiskType.LOCAL => "local-ssd"
  }
}
