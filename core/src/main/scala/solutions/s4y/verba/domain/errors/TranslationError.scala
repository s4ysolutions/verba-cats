package solutions.s4y.verba.domain.errors

import cats.data.NonEmptyChain
import cats.implicits.*

enum TranslationError:
  case RequestValidation(errors: NonEmptyChain[RequestValidationError])
  case Api(error: ApiError)

  def category: String = this match
    case TranslationError.RequestValidation(_) => "Validation"
    case TranslationError.Api(_)               => "API"

  def message: String = this match
    case TranslationError.RequestValidation(errors) =>
      s"Request errors: ${errors.map(_.message).toList.mkString(", ")}"
    case TranslationError.Api(err) => err.message

  def cause: Option[Throwable] = this match
    case TranslationError.Api(err) => err.cause
    case _                         => None
