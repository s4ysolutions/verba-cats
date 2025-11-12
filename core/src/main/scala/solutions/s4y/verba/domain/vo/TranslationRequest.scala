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
    sourceLang: Option[String],
    targetLang: String,
    mode: TranslationMode,
    provider: TranslationProvider,
    quality: TranslationQuality,
    ipa: Boolean
)

object TranslationRequest:
  private val textArg = "text"
  private val fromArg = "from"
  private val toArg = "to"
  private val modeArg = "mode"
  private val providerArg = "provider"
  private val qualityArg = "quality"
  private val ipaArg = "ipa"

  type ValidationResult[A] = ValidatedNec[RequestValidationError, A]

  def apply(
      sourceText: Option[String],
      sourceLang: Option[String],
      targetLang: Option[String],
      mode: Option[String],
      provider: Option[String],
      quality: Option[String],
      ipa: Option[Boolean]
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

    val vSourceLang: ValidationResult[Option[String]] = sourceLang
      .map(_.trim)
      .fold(
        (None: Option[String]).validNec
        //RequestValidationError.EmptyString(fromArg).invalidNec
      ) { lang =>
        if (lang.isEmpty)
            (None: Option[String]).validNec
          // RequestValidationError.EmptyString(fromArg).invalidNec
        else if (lang.length > 12)
          RequestValidationError.LangTooLong(lang).invalidNec
        else if (lang.length <= 2)
          RequestValidationError.LangTooShort(lang).invalidNec
        else Some(lang).validNec
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

    val ipaBool = ipa.getOrElse(false)
    val validationResult: ValidationResult[TranslationRequest] =
      (vText, vSourceLang, vTargetLang, vMode, vProvider, vQuality)
        .mapN { (text, srcL, trgtL, mode, provider, quality) =>
          TranslationRequest(
            Prompt(text, mode, srcL, trgtL, ipaBool),
            srcL,
            trgtL,
            mode,
            provider,
            quality,
            ipaBool
          )
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
      quality = options.get(qualityArg),
      ipa = options.get(ipaArg).map(s => s.toLowerCase == "true")
    )
