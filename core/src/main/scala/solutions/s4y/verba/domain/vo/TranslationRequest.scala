package solutions.s4y.verba.domain.vo

import cats.data.ValidatedNec
import cats.syntax.all.*
import solutions.s4y.verba.domain.errors.{
  RequestValidationError,
  TranslationError
}

final case class TranslationRequest private (
    // sourceText: String,
    prompt: Prompt,
    sourceLang: String,
    targetLang: String,
    mode: TranslationMode,
    provider: TranslationProvider,
    quality: TranslationQuality
)

object TranslationRequest:
  val textArg = "text"
  val fromArg = "from"
  val toArg = "to"
  val modeArg = "mode"
  val providerArg = "provider"
  val qualityArg = "quality"

  type ValidationResult[A] = ValidatedNec[RequestValidationError, A]

  def apply(
      sourceText: Option[String],
      sourceLang: Option[String],
      targetLang: Option[String],
      mode: Option[String],
      provider: Option[String],
      quality: Option[String]
  ): Either[TranslationError.RequestValidation, TranslationRequest] =

    val vText: ValidationResult[String] = sourceText
      .map(_.trim)
      .fold(
        RequestValidationError.EmptyString(textArg).invalidNec
      ) { text =>
        if text.isEmpty then
          RequestValidationError.EmptyString(textArg).invalidNec
        else text.validNec
      }

    val vSourceLang: ValidationResult[String] = sourceLang
      .map(_.trim)
      .fold(
        RequestValidationError.EmptyString(fromArg).invalidNec
      ) { lang =>
        if (lang.isEmpty)
          RequestValidationError.EmptyString(fromArg).invalidNec
        else if (lang.length > 12)
          RequestValidationError.LangTooLong(lang).invalidNec
        else if (lang.length <= 2)
          RequestValidationError.LangTooShort(lang).invalidNec
        else lang.validNec
      }

    val vTargetLang: ValidationResult[String] = targetLang
      .map(_.trim)
      .fold(
        RequestValidationError.EmptyString(toArg).invalidNec
      ) { lang =>
        if (lang.isEmpty)
          RequestValidationError.EmptyString(toArg).invalidNec
        else if (lang.length > 12)
          RequestValidationError.LangTooLong(lang).invalidNec
        else if (lang.length <= 2)
          RequestValidationError.LangTooShort(lang).invalidNec
        else lang.validNec
      }

    val vMode: ValidationResult[TranslationMode] =
      TranslationMode.fromString(mode.getOrElse("auto"))

    val vProvider: ValidationResult[TranslationProvider] =
      TranslationProvider.fromString(provider.getOrElse("gemini"))

    val vQuality: ValidationResult[TranslationQuality] =
      TranslationQuality.fromString(quality.getOrElse("optimal"))

    val validationResult: ValidationResult[TranslationRequest] =
      (vText, vSourceLang, vTargetLang, vMode, vProvider, vQuality)
        .mapN { (t, s, ta, m, p, q) =>
          TranslationRequest(Prompt(t, m, s, ta), s, ta, m, p, q)
        }

    validationResult.toEither.leftMap(errs =>
      TranslationError.RequestValidation(errs)
    )

  def apply(
      options: Map[String, String]
  ): Either[TranslationError.RequestValidation, TranslationRequest] =
    apply(
      sourceText = options.get(textArg),
      sourceLang = options.get(fromArg),
      targetLang = options.get(toArg),
      mode = options.get(modeArg),
      provider = options.get(providerArg),
      quality = options.get(qualityArg)
    )
