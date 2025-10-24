package solutions.s4y.verba.domain.vo

import cats.data.ValidatedNec
import cats.syntax.all.*
import solutions.s4y.verba.domain.errors.RequestValidationError

enum TranslationMode:
  case TranslateSentence
  case ExplainWords
  case Auto

object TranslationMode:
  def fromString(raw: String): ValidatedNec[RequestValidationError, TranslationMode] =
    val normalized = Option(raw).getOrElse("").trim.toLowerCase
    normalized match
      case "translatesentence" | "translate_sentence" | "sentence" | "translate" =>
        TranslateSentence.validNec
      case "explainwords" | "explain_words" | "explain" | "words" | "word" =>
        ExplainWords.validNec
      case "auto" | "" =>
        Auto.validNec
      case _ =>
        RequestValidationError.InvalidMode(raw).invalidNec