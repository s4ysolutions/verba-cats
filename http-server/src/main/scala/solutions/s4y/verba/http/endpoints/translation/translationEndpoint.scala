package solutions.s4y.verba.http.endpoints.translation

import cats.data.EitherT
import cats.effect.IO
import cats.implicits.*
import io.circe.generic.auto.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import scribe.cats.LoggerExtras
import scribe.{Logger, Scribe}
import solutions.s4y.verba.domain.vo.TranslationRequest
import solutions.s4y.verba.usecases.TranslatorService

def translationEndpoint(
    translationService: TranslatorService,
    logger: Logger
): HttpRoutes[IO] = {
  val loggerIO = logger.f[IO]
  HttpRoutes.of[IO] { case req @ POST -> Root / "translation" =>
    val result = for {
      _ <- EitherT.right[String](loggerIO.debug("Received translation request"))
      dto <- EitherT(req.as[TranslationRequestDto].attempt)
        .leftMap((err: Throwable) => s"Invalid JSON: ${err.getMessage}")
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
      translation <- EitherT(translationService.translate(translationReq))
        .leftMap(err => err.message)
      _ <- EitherT.right[String](
        loggerIO.debug("Translation completed successfully")
      )
    } yield translation

    result.value.flatMap {
      case Right(translation) =>
        Ok(translation)
      case Left(errorMsg) =>
        BadRequest(errorMsg)
    }
  }
}
