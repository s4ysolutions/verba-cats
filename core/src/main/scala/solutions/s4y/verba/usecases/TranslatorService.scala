package solutions.s4y.verba.usecases

import cats.effect.{IO, Temporal}
import solutions.s4y.verba.domain.errors.{ApiError, TranslationError}
import solutions.s4y.verba.domain.vo.{TranslationProvider, TranslationRequest}
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
  ): IO[Either[TranslationError, String]] =
    val repo = request.provider match
      case TranslationProvider.OpenAI => openAiRepository
      case TranslationProvider.Gemini => geminiRepository

    val translationEffect = repo.translate(request)

    retryEffect(translationEffect, attemptNumber = 1)
  end translate

  private def retryEffect(
      effect: IO[Either[TranslationError, String]],
      attemptNumber: Int
  ): IO[Either[TranslationError, String]] =
    effect.flatMap {
      case Right(result) => IO.pure(Right(cleanupResult(result)))
      case Left(TranslationError.Api(ApiError.TemporaryUnavailable))
          if attemptNumber < maxRetries =>
        val delay = baseRetryDelay * attemptNumber
        Temporal[IO].sleep(delay) *> retryEffect(effect, attemptNumber + 1)
      case Left(error) => IO.pure(Left(error))
    }
  end retryEffect

  private def cleanupResult(str: String): String =
    val leftTrimmed =
      str.dropWhile(c => c.isWhitespace || extraTrimChars.contains(c))
    leftTrimmed.reverse
      .dropWhile(c => c.isWhitespace || extraTrimChars.contains(c))
      .reverse

  private val extraTrimChars: Set[Char] =
    Set('"', '«', '»', '‘', '’', '“', '”')
