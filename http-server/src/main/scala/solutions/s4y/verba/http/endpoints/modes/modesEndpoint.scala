package solutions.s4y.verba.http.endpoints.modes

import cats.effect.IO
import io.circe.syntax.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import solutions.s4y.verba.usecases.TranslatorService

def modesEndpoint(translatorService: TranslatorService): HttpRoutes[IO] =
  HttpRoutes.of[IO] { case GET -> Root / "modes" =>
    translatorService.modesSupported.flatMap {
      case Right(modes) =>
        Ok(modes.map(_.toString).toList.asJson)
      case Left(_) =>
        InternalServerError("Failed to retrieve supported modes")
    }
  }

