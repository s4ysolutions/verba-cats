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
    val options: Map[String, String] =
      nextOption(Map.empty[String, String], args)

    val request: Either[RequestValidation, TranslationRequest] =
      TranslationRequest(options)

    val resultIO: EitherT[IO, TranslationError, String] = for {
      req: TranslationRequest <- EitherT.fromEither[IO](request)
      res: String <- EitherT(
        translator.translate(req)
      )
    } yield res

    val resultIOValue: IO[Either[TranslationError, String]] = resultIO.value

    resultIOValue.flatMap {
      case Left(err: RequestValidation) =>
        IO.println(s"${err.message}\n$usage").as(ExitCode.Error)
      case Left(err) =>
        IO.println(err.message).as(ExitCode.Error)
      case Right(text) =>
        IO.println(text).as(ExitCode.Success)
    }

  end run

  private val translator =
    TranslatorService(OpenAIRepository(), GeminiRepository())

  private def isSwitch(s: String): Boolean = s.startsWith("-")
  // TODO: args should be synced with TranslationRequest fields somehow
  @tailrec
  private def nextOption(
      map: Map[String, String],
      list: List[String]
  ): Map[String, String] =
    list match {
      case Nil                       => map
      case "--from" :: value :: tail =>
        nextOption(map ++ Map("from" -> value), tail)
      case "-f" :: value :: tail =>
        nextOption(map ++ Map("from" -> value), tail)
      case "--to" :: value :: tail =>
        nextOption(map ++ Map("to" -> value), tail)
      case "-t" :: value :: tail =>
        nextOption(map ++ Map("to" -> value), tail)
      case "--mode" :: value :: tail =>
        nextOption(map ++ Map("mode" -> value), tail)
      case "-m" :: value :: tail =>
        nextOption(map ++ Map("mode" -> value), tail)
      case "--quality" :: value :: tail =>
        nextOption(map ++ Map("quality" -> value), tail)
      case "-q" :: value :: tail =>
        nextOption(map ++ Map("quality" -> value), tail)
      case "--provider" :: value :: tail =>
        nextOption(map ++ Map("provider" -> value), tail)
      case "-p" :: value :: tail =>
        nextOption(map ++ Map("provider" -> value), tail)
      // Handle positional <text> (last non-switch arg)
      case string :: opt2 :: tail if !isSwitch(string) && isSwitch(opt2) =>
        nextOption(map ++ Map("text" -> string), list.tail)
      case string :: Nil if !isSwitch(string) =>
        nextOption(map ++ Map("text" -> string), Nil)
      case option :: tail =>
        println(s"Unknown option: $option")
        println(usage)
        sys.exit(1)
    }
  end nextOption

  private val usage = """
      Usage: verba [-f|--from <lang>] [-t|--to <lang>] [-m|--mode <mode>] [-q|--quality <quality>] [-p|--provider <provider>] <text>

      Options:
        -f, --from <lang>     Source language (e.g., en, fr) (required)
        -t, --to <lang>       Target language (e.g., en, fr) (required)
        -m, --mode <mode>     Mode: translate, explain, auto (default: auto)
        -q, --quality <quality> Quality: fast, optimal, thinking (default: optimal)
        -p, --provider <provider> Translation provider: openai, gemini (default: openai)

      <text>                 Text to translate (required)
    """.trim
end Main
