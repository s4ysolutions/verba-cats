package solutions.s4y.verba.adapters

import cats.effect.{IO, Temporal}
import cats.syntax.all.*
import scala.concurrent.duration.*
import org.http4s.*
import org.http4s.client.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.circe.*
import io.circe.Json
import io.circe.parser.*
import solutions.s4y.verba.ports.driven.TranslationRepository
import solutions.s4y.verba.domain.errors.{TranslationError, ApiError}
import solutions.s4y.verba.domain.vo.{
  TranslationRequest,
  TranslationQuality,
  Prompt
}

class GeminiRepository extends TranslationRepository:
  private val baseUrl = "https://generativelanguage.googleapis.com/v1beta"

  private val modelFast: String = "models/gemini-flash-lite-latest"
  private val modelOptimal: String = "models/gemini-flash-latest"
  private val modelThinking: String = "models/gemini-2.5-pro"

  def translate(
      request: TranslationRequest
  ): IO[Either[TranslationError, String]] =
    APIConfig.geminiAPIKey match
      case None => IO.pure(Left(TranslationError.Api(ApiError.InvalidKey)))
      case Some(apiKey) =>
        val modelName = request.quality match
          case TranslationQuality.Fast     => modelFast
          case TranslationQuality.Optimal  => modelOptimal
          case TranslationQuality.Thinking => modelThinking

        val prompt = Prompt.derive(request)

        val url = Uri.unsafeFromString(
          s"$baseUrl/$modelName:generateContent?key=$apiKey"
        )

        val bodyJson: Json = Json.obj(
          "contents" -> Json.arr(
            Json.obj(
              "parts" -> Json.arr(Json.obj("text" -> Json.fromString(prompt)))
            )
          ),
          "generationConfig" -> Json.obj(
            "temperature" -> Json.fromDoubleOrNull(1.0)
          )
        )

        val req = Request[IO](Method.POST, url)
          .withHeaders(
            Header.Raw(
              org.typelevel.ci.CIString("Content-Type"),
              "application/json"
            )
          )
          .withEntity(bodyJson)

        EmberClientBuilder.default[IO].build.use { client =>
          client.run(req).use { resp =>
            val code = resp.status.code
            if code == 200 then
              resp.as[String].attempt.flatMap {
                case Left(err) =>
                  IO.pure(Left(TranslationError.Api(ApiError.Networking(err))))
                case Right(bodyStr) =>
                  parse(bodyStr) match
                    case Left(err) =>
                      IO.pure(
                        Left(
                          TranslationError
                            .Api(ApiError.DecodingFailed(bodyStr, err))
                        )
                      )
                    case Right(json) =>
                      val textOpt = for
                        obj <- json.asObject
                        candidatesJson <- obj("candidates")
                        candidatesArr <- candidatesJson.asArray
                        first <- candidatesArr.headOption
                        firstObj <- first.asObject
                        contentJson <- firstObj("content")
                        contentObj <- contentJson.asObject
                        partsJson <- contentObj("parts")
                        partsArr <- partsJson.asArray
                        firstPart <- partsArr.headOption
                        firstPartObj <- firstPart.asObject
                        textJson <- firstPartObj("text")
                        textStr <- textJson.asString
                      yield textStr

                      textOpt match
                        case Some(text) => IO.pure(Right(text.trim))
                        case None       =>
                          IO.pure(
                            Left(
                              TranslationError.Api(
                                ApiError.Unexpected(
                                  s"Unexpected response format: $bodyStr"
                                )
                              )
                            )
                          )
              }
            else if code == 429 then
              IO.pure(Left(TranslationError.Api(ApiError.RateLimitExceeded)))
            else if code == 503 then
              IO.pure(Left(TranslationError.Api(ApiError.TemporaryUnavailable)))
            else
              resp.as[String].attempt.flatMap {
                case Left(err) =>
                  IO.pure(Left(TranslationError.Api(ApiError.Networking(err))))
                case Right(bodyStr) =>
                  IO.pure(
                    Left(
                      TranslationError
                        .Api(ApiError.Unexpected(s"HTTP $code: $bodyStr"))
                    )
                  )
              }
          }
        }

object GeminiRepository:
  def apply(): GeminiRepository = new GeminiRepository()
