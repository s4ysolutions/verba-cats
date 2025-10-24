package solutions.s4y.verba.http

import cats.effect.*
import cats.implicits.*
import org.http4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.Logger as RequestLogger
import scribe.cats.LoggerExtras
import solutions.s4y.verba.adapters.{GeminiRepository, OpenAIRepository}
import solutions.s4y.verba.http.endpoints.translation.translationEndpoint
import solutions.s4y.verba.scribe.ScribeLogger
import solutions.s4y.verba.usecases.TranslatorService

object Main extends IOApp {
  private val httpLogger = scribe.Logger("verba.http4s").f[IO]
  private val emberLogger = ScribeLogger.getLogger[IO]

  private val translatorService =
    TranslatorService(OpenAIRepository(), GeminiRepository())

  def run(args: List[String]): IO[ExitCode] = {
    def translationApp: HttpApp[IO] =
      RequestLogger.httpApp(
        logHeaders = true,
        logBody = false,
        redactHeadersWhen = _ => false,
        logAction = Some((msg: String) => httpLogger.info(msg))
      )(translationEndpoint(translatorService).orNotFound)

    ServerConfig.fromArgs(args) match {
      case Left(error) =>
        IO.println(error).as(ExitCode.Error)
      case Right(config) =>
        httpLogger.info(
          s"Starting server on ${config.host}:${config.port}"
        ) *>
          EmberServerBuilder
            .default[IO]
            .withHost(config.host)
            .withPort(config.port)
            .withHttpApp(translationApp)
            .withLogger(emberLogger)
            .build
            .use(_ =>
              httpLogger.info(
                s"Server is listening on ${config.host}:${config.port}"
              ) *> IO.never
            )
            .as(ExitCode.Success)
            .handleErrorWith { err =>
              httpLogger
                .error(s"Server failed to start: ${err.getMessage}")
                .as(ExitCode.Error)
            }
    }
  }
}
