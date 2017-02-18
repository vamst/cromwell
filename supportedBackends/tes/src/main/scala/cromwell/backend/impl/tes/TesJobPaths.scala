package cromwell.backend.impl.tes

import com.typesafe.config.Config
import cromwell.backend.io.{JobPaths, WorkflowPaths}
import cromwell.backend.{BackendJobDescriptorKey, BackendWorkflowDescriptor}
import cromwell.core.path.Obsolete._
import cromwell.core.path.{Path, PathBuilder}

class TesJobPaths(val jobKey: BackendJobDescriptorKey,
                  workflowDescriptor: BackendWorkflowDescriptor,
                  config: Config,
                  pathBuilders: List[PathBuilder] = WorkflowPaths.DefaultPathBuilders) extends TesWorkflowPaths(
  workflowDescriptor, config, pathBuilders) with JobPaths {

  import JobPaths._

  override lazy val callExecutionRoot = {
    callRoot.resolve("execution")
  }
  val callDockerRoot = callPathBuilder(dockerWorkflowRoot, jobKey)
  val callExecutionDockerRoot = callDockerRoot.resolve("execution")
  val callInputsDockerRoot = callDockerRoot.resolve("inputs")
  val callInputsRoot = callRoot.resolve("inputs")

  //TODO move to TesConfiguration
  private def prefixScheme(path: String): String = "file://" + path

  def storageInput(path: String): String = prefixScheme(path)

  // Given an output path, return a path localized to the storage file system
  def storageOutput(path: String): String = {
    prefixScheme(callExecutionRoot.resolve(path).toString)
  }

  def containerInput(path: String): String = {
    cleanContainerInputPath(callInputsDockerRoot, Paths.get(path))
  }

  // Given an output path, return a path localized to the container file system
  def containerOutput(cwd: Path, path: String): String = containerExec(cwd, path)

  // TODO this could be used to create a separate directory for outputs e.g.
  // callDockerRoot.resolve("outputs").resolve(name).toString

  // Given an file name, return a path localized to the container's execution directory
  def containerExec(cwd: Path, path: String): String = {
    cwd.resolve(path).toString
  }

  private def cleanContainerInputPath(inputDir: Path, path: Path): String = {
    path.toAbsolutePath match {
      case p if p.startsWith(callExecutionRoot) =>
        callExecutionDockerRoot.resolve(p.getFileName.toString).toString
      case p =>
        inputDir + p.toString
    }
  }
}