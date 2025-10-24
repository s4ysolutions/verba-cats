package solutions.s4y.verba.domain.vo

import cats.data.ValidatedNec
import cats.syntax.all.*
import solutions.s4y.verba.domain.errors.RequestValidationError

enum TranslationQuality:
  case Fast
  case Optimal
  case Thinking

object TranslationQuality:
  def fromString(
      raw: String
  ): ValidatedNec[RequestValidationError, TranslationQuality] =
    val normalized = Option(raw).getOrElse("").trim.toLowerCase
    normalized match
      case "fast" | "low" | "small"                  => Fast.validNec
      case "optimal" | "middle" | "moderate" | "big" => Optimal.validNec
      case "high" | "think" | "thinking" | "deep"    => Thinking.validNec
      case _ => RequestValidationError.InvalidQuality(raw).invalidNec
