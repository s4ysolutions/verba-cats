package solutions.s4y.verba.usecases

import cats.effect.{IO, Temporal}
import solutions.s4y.verba.domain.errors.{ApiError, TranslationError}
import solutions.s4y.verba.domain.vo.*
import solutions.s4y.verba.ports.driven.TranslationRepository

import scala.concurrent.duration.*

class TranslatorService(
    openAiRepository: TranslationRepository,
    geminiRepository: TranslationRepository,
    maxRetries: Int = 3,
    baseRetryDelay: FiniteDuration = 1.second
):
  def translate(
      request: TranslationRequest
  ): IO[Either[TranslationError, TranslationResponse]] =
    val repo = request.provider match
      case TranslationProvider.OpenAI => openAiRepository
      case TranslationProvider.Gemini => geminiRepository

    val translationEffect = repo.translate(request)

    retryEffect(translationEffect, attemptNumber = 1)
  end translate

  def modesSupported: IO[Either[Nothing, Set[TranslationMode]]] =
    IO.pure(
      Right(
        Set(
          TranslationMode.TranslateSentence,
          TranslationMode.ExplainWords,
          TranslationMode.Auto
        )
      )
    )
  end modesSupported

  def providersSupported: IO[Either[Nothing, Set[TranslationProvider]]] =
    IO.pure(Right(Set(TranslationProvider.OpenAI, TranslationProvider.Gemini)))
  end providersSupported

  def qualitiesSupported: IO[Either[Nothing, Set[TranslationQuality]]] =
    IO.pure(
      Right(
        Set(
          TranslationQuality.Fast,
          TranslationQuality.Optimal,
          TranslationQuality.Thinking
        )
      )
    )
  end qualitiesSupported

  private def retryEffect(
      effect: IO[Either[TranslationError, TranslationResponse]],
      attemptNumber: Int
  ): IO[Either[TranslationError, TranslationResponse]] =
    effect.flatMap {
      case Right(result) => IO.pure(Right(cleanupResult(result)))
      case Left(TranslationError.Api(ApiError.TemporaryUnavailable))
          if attemptNumber < maxRetries =>
        val delay = baseRetryDelay * attemptNumber
        Temporal[IO].sleep(delay) *> retryEffect(effect, attemptNumber + 1)
      case Left(error) => IO.pure(Left(error))
    }
  end retryEffect

  private def cleanupResult(
      response: TranslationResponse
  ): TranslationResponse =
    val leftTrimmed =
      response.text.dropWhile(c => c.isWhitespace || extraTrimChars.contains(c))

    TranslationResponse(
      leftTrimmed.reverse
        .dropWhile(c => c.isWhitespace || extraTrimChars.contains(c))
        .reverse,
      response.promptTokenCount,
      response.textTokenCount
    )

  private val extraTrimChars: Set[Char] =
    Set('"', '«', '»', '‘', '’', '“', '”')
