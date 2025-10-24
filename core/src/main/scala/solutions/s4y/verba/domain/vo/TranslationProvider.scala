package solutions.s4y.verba.domain.vo

import cats.data.ValidatedNec
import cats.syntax.all.*
import solutions.s4y.verba.domain.errors.RequestValidationError

enum TranslationProvider:
  case OpenAI
  case Gemini

object TranslationProvider:
  def fromString(raw: String): ValidatedNec[RequestValidationError, TranslationProvider] =
    val normalized = Option(raw).getOrElse("").trim.toLowerCase
    normalized match
      case "openai"        => OpenAI.validNec
      case "google" | "gemini" => Gemini.validNec
      case _               => RequestValidationError.InvalidProvider(raw).invalidNec