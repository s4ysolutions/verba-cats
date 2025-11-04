package solutions.s4y.verba.http.endpoints.providers

import cats.effect.IO
import io.circe.syntax.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import solutions.s4y.verba.usecases.TranslatorService

def providersEndpoint(translatorService: TranslatorService): HttpRoutes[IO] =
  HttpRoutes.of[IO] { case GET -> Root / "providers" =>
    translatorService.providersSupported.flatMap {
      case Right(providers) =>
        Ok(providers.map(_.toString).toList.asJson)
      case Left(_) =>
        InternalServerError("Failed to retrieve supported providers")
    }
  }

