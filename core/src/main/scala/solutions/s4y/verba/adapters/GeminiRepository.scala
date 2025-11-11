package solutions.s4y.verba.adapters

import cats.effect.IO
import io.circe.Json
import io.circe.parser.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.ember.client.EmberClientBuilder
import solutions.s4y.verba.domain.errors.{ApiError, TranslationError}
import solutions.s4y.verba.domain.vo.{TranslationQuality, TranslationRequest}
import solutions.s4y.verba.ports.driven.TranslationRepository

class GeminiRepository extends TranslationRepository:
  private val baseUrl = "https://generativelanguage.googleapis.com/v1beta"

  private val modelFast: String = "models/gemini-2.5-flash-lite"
  private val modelOptimal: String = "models/gemini-2.5-flash"
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

        val prompt = request.prompt.value

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

        EmberClientBuilder
          .default[IO]
          .build
          .use { client =>
            client.run(req).use { resp =>
              val code = resp.status.code
              if code == 200 then
                resp.as[String].attempt.flatMap {
                  case Left(err) =>
                    IO.pure(
                      Left(TranslationError.Api(ApiError.Networking(err)))
                    )
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
                IO.pure(
                  Left(TranslationError.Api(ApiError.TemporaryUnavailable))
                )
              else
                resp.as[String].attempt.flatMap {
                  case Left(err) =>
                    IO.pure(
                      Left(TranslationError.Api(ApiError.Networking(err)))
                    )
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
          .handleErrorWith {
            case e: java.util.concurrent.TimeoutException =>
              IO.pure(Left(TranslationError.Api(ApiError.Timeout)))
            case e =>
              IO.pure(Left(TranslationError.Api(ApiError.Networking(e))))
          }

object GeminiRepository:
  def apply(): GeminiRepository = new GeminiRepository()
