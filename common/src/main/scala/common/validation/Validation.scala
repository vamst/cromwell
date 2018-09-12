package common.validation

import java.io.{PrintWriter, StringWriter}

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.either._
import cats.syntax.validated._
import com.typesafe.config.Config
import common.Checked
import common.exception.AggregatedMessageException
import common.validation.ErrorOr.ErrorOr
import common.validation.Parse.Parse
import eu.timepit.refined.api.Refined
import eu.timepit.refined.refineV
import net.ceedubs.ficus.readers.ValueReader
import org.slf4j.Logger

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Validation {
  def warnNotRecognized(keys: Set[String], reference: Set[String], context: String, logger: Logger): Unit = {
    val unrecognizedKeys = keys.diff(reference)
    if (unrecognizedKeys.nonEmpty) {
      logger.warn(s"Unrecognized configuration key(s) for $context: ${unrecognizedKeys.mkString(", ")}")
    }
  }

  def validate[A](block: => A): ErrorOr[A] = Try(block) match {
    case Success(result) => result.validNel
    case Failure(f) => f.getMessage.invalidNel
  }

  implicit class ValidationOps[B,A](val v: ValidatedNel[B, A]) {
    //Convert this into a future by folding over the state and returning the corresponding Future terminal state.
    def toFuture(f: NonEmptyList[B] => Throwable) =
      v fold(
        //Use f to turn the failure list into a Throwable, then fail a future with it.
        //Function composition lets us ignore the actual argument of the error list
        Future.failed _ compose f,
        Future.successful
      )
  }

  implicit class TryValidation[A](val t: Try[A]) extends AnyVal {
    def toErrorOr: ErrorOr[A] = {
      Validated.fromTry(t).leftMap(_.getMessage).toValidatedNel[String, A]
    }

    def toErrorOrWithContext(context: String): ErrorOr[A] = toChecked
      .contextualizeErrors(context)
      .leftMap({contextualizedErrors =>
        if (t.failed.isFailure) {
          val errors = new StringWriter
          t.failed.get.printStackTrace(new PrintWriter(errors))
          contextualizedErrors.::(s"Stacktrace: ${errors.toString}")
        } else contextualizedErrors
      })
      .toValidated

    def toChecked: Checked[A] = {
      Either.fromTry(t).leftMap(ex => NonEmptyList.of(ex.getMessage))
    }
  }

  implicit class ValidationTry[A](val e: ErrorOr[A]) extends AnyVal {
    def toTry: Try[A] = toTry("Error(s)")

    def toTry(context: String): Try[A] = e match {
      case Valid(options) => Success(options)
      case Invalid(err) => Failure(AggregatedMessageException(context, err.toList))
    }

    def unsafe: A = unsafe("Errors(s)")

    def unsafe(context: String): A = e.valueOr(errors => throw AggregatedMessageException(context, errors.toList))
  }

  implicit class ValidationChecked[A](val e: Checked[A]) extends AnyVal {
    def unsafe(context: String): A = e.valueOr(errors => throw AggregatedMessageException(context, errors.toList))

    def contextualizeErrors(s: => String): Checked[A] = e.leftMap { errors =>
      val total = errors.size
      errors.zipWithIndex map { case (err, i) => s"Failed to $s (reason ${i + 1} of $total): $err" }
    }

  }

  implicit class OptionValidation[A](val o: Option[A]) extends AnyVal {
    def toErrorOr(errorMessage: String): ErrorOr[A] = {
      Validated.fromOption(o, NonEmptyList.of(errorMessage))
    }

    def toChecked(errorMessage: String): Checked[A] = {
      Either.fromOption(o, NonEmptyList.of(errorMessage))
    }

    def toParse(errorMessage: String): Parse[A] = {
      Parse.checkedParse(Either.fromOption(o, NonEmptyList.of(errorMessage)))
    }
  }

  implicit def refinedValueReader[A, P](implicit reader: ValueReader[A], validation: eu.timepit.refined.api.Validate[A, P]) = new ValueReader[Refined[A, P]] {
    override def read(config: Config, path: String) = {
      val unCheckedValue = reader.read(config, path)
      refineV[P](unCheckedValue)
        .leftMap(NonEmptyList.one)
        .unsafe(s"Configuration value $unCheckedValue at $path does not satisfy predicate constraint: ${validation.showExpr(unCheckedValue)}")
    }
  }
}
