package solutions.s4y.verba.http.endpoints.translation

import cats.data.EitherT
import cats.effect.{Clock, IO}
import cats.implicits.*
import io.circe.generic.auto.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import scribe.Logger
import scribe.cats.LoggerExtras
import solutions.s4y.verba.domain.vo.TranslationRequest
import solutions.s4y.verba.usecases.TranslatorService

import scala.concurrent.duration.Duration

def translationEndpoint(
    translationService: TranslatorService,
    logger: Logger
): HttpRoutes[IO] = {
  val loggerIO = logger.f[IO]
  HttpRoutes.of[IO] { case req @ POST -> Root / "translation" =>
    val result = for {
      _ <- EitherT.right[String](loggerIO.debug("Received translation request"))
      dto <- EitherT(req.as[TranslationRequestDto].attempt)
        .leftMap((err: Throwable) => err.getMessage)
      _ <- EitherT.right[String](loggerIO.debug(s"Request DTO: $dto"))
      translationReq <- EitherT.fromEither[IO](
        TranslationRequest(
          sourceText = Some(dto.text),
          sourceLang = dto.from,
          targetLang = Some(dto.to),
          mode = dto.mode,
          provider = dto.provider,
          quality = dto.quality,
          ipa = dto.ipa
        ).leftMap(err => err.message)
      )
      _ <- EitherT.right[String](
        loggerIO.debug(s"Translating from ${dto.from} to ${dto.to}")
      )
      startTime <- EitherT.right(Clock[IO].realTimeInstant)
      translation <- EitherT(translationService.translate(translationReq))
        .leftMap(err => err.message)
      endTime <- EitherT.right(Clock[IO].realTimeInstant)
      durationMs = (endTime.toEpochMilli - startTime.toEpochMilli)
    } yield TranslationResponseDto(
      translation.text,
      translation.promptTokenCount,
      translation.textTokenCount,
      durationMs
    )

    result.value.flatMap {
      case Right(response) =>
        loggerIO.debug("Translation completed successfully") *>
          Ok(response)
      case Left(errorMsg) =>
        loggerIO.error(s"Translation failed: $errorMsg") *>
          BadRequest(errorMsg)
    }
  }
}
