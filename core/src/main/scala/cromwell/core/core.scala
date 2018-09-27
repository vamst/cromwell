package cromwell.core

import com.typesafe.config.Config
import common.exception.ThrowableAggregation
import cromwell.core.path.Path

import scala.concurrent.duration.FiniteDuration


case class StandardPaths(output: Path, error: Path)

case class CallContext(root: Path, standardPaths: StandardPaths, isDocker: Boolean)

/**  Marker trait for Cromwell exceptions that are to be treated as fatal (non-retryable) */
trait CromwellFatalExceptionMarker { this: Throwable => }

object CromwellFatalException {
  // Don't wrap if it's already a fatal exception
  def apply(throwable: Throwable) = throwable match {
    case e: CromwellFatalExceptionMarker => e
    case e => new CromwellFatalException(e)
  }
  def unapply(e: CromwellFatalException): Option[Throwable] = Option(e.exception)
}

class CromwellFatalException(val exception: Throwable) extends Exception(exception) with CromwellFatalExceptionMarker

case class CromwellAggregatedException(throwables: Seq[Throwable], exceptionContext: String = "")
  extends Exception with ThrowableAggregation

case class CacheConfig(concurrency: Int, size: Long, ttl: FiniteDuration)

import net.ceedubs.ficus.Ficus._
object CacheConfig {
  def fromConfig(caching: Config, defaultConcurrency: Int, defaultSize: Long, defaultTtl: FiniteDuration): CacheConfig = {
    val ttl = caching.as[Option[FiniteDuration]]("ttl").getOrElse(defaultTtl)
    val concurrency = caching.as[Option[Int]]("concurrency").getOrElse(defaultConcurrency)
    val size = caching.as[Option[Long]]("size").getOrElse(defaultSize)
    CacheConfig(concurrency = concurrency, size = size, ttl = ttl)
  }
}
