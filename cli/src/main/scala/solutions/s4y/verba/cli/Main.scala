package solutions.s4y.verba.cli

import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*
import solutions.s4y.verba.adapters.{GeminiRepository, OpenAIRepository}
import solutions.s4y.verba.domain.errors.TranslationError
import solutions.s4y.verba.domain.errors.TranslationError.RequestValidation
import solutions.s4y.verba.domain.vo.TranslationRequest
import solutions.s4y.verba.usecases.TranslatorService

import scala.annotation.tailrec

object Main extends IOApp:

  def run(args: List[String]): IO[ExitCode] =
    val options: Map[String, String] = Args.parseArgs(args)

    if (options.contains("help")) {
      return IO.println(Args.usage).as(ExitCode.Error)
    }

    val request: Either[RequestValidation, TranslationRequest] =
      TranslationRequest(options)

    val resultIO: EitherT[IO, TranslationError, String] = for {
      req: TranslationRequest <- EitherT.fromEither[IO](request)
      res <- EitherT(
        translator.translate(req)
      )
    } yield res.translated

    val resultIOValue: IO[Either[TranslationError, String]] = resultIO.value

    resultIOValue.flatMap {
      case Left(err: RequestValidation) =>
        IO.println(Args.usage).as(ExitCode.Error)
      case Left(err) =>
        IO.println(err.message).as(ExitCode.Error)
      case Right(text) =>
        IO.println(text).as(ExitCode.Success)
    }

  end run

  private val translator =
    TranslatorService(OpenAIRepository(), GeminiRepository())

end Main
