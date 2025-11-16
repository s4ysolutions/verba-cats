package solutions.s4y.verba.adapters

import cats.effect.IO
import io.circe.Json
import io.circe.parser.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.ci.CIStringSyntax
import solutions.s4y.verba.domain.errors.{ApiError, TranslationError}
import solutions.s4y.verba.domain.vo.{
  TranslationQuality,
  TranslationRequest,
  TranslationResponse
}
import solutions.s4y.verba.ports.driven.TranslationRepository

class OpenAIRepository extends TranslationRepository:
  private val url = "https://api.openai.com/v1/chat/completions"

  private val modelFast: String = "gpt-4.1-nano"
  private val modelOptimal: String = "gpt-4.1-mini"
  private val modelThinking: String = "gpt-4.1"

  def translate(
      request: TranslationRequest
  ): IO[Either[TranslationError, TranslationResponse]] =
    APIConfig.openAIAPIKey match
      case None => IO.pure(Left(TranslationError.Api(ApiError.InvalidKey)))
      case Some(apiKey) =>
        val modelName = request.quality match
          case TranslationQuality.Fast     => modelFast
          case TranslationQuality.Optimal  => modelOptimal
          case TranslationQuality.Thinking => modelThinking

        val prompt = request.prompt.value

        val bodyJson: Json = Json.obj(
          "model" -> Json.fromString(modelName),
          "messages" -> Json.arr(
            Json.obj(
              "role" -> Json.fromString("user"),
              "content" -> Json.fromString(prompt)
            )
          ),
          "temperature" -> Json.fromInt(1)
        )

        val req = Request[IO](Method.POST, Uri.unsafeFromString(url))
          .withHeaders(
            Header.Raw(ci"Authorization", s"Bearer $apiKey"),
            Header.Raw(ci"Content-Type", "application/json")
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
                        val contentOpt = for
                          obj <- json.asObject
                          choicesJson <- obj("choices")
                          choicesArr <- choicesJson.asArray
                          first <- choicesArr.headOption
                          firstObj <- first.asObject
                          messageJson <- firstObj("message")
                          messageObj <- messageJson.asObject
                          contentJson <- messageObj("content")
                          contentStr <- contentJson.asString
                        yield contentStr

                        contentOpt match
                          case Some(content) =>
                            val usage = for
                              obj <- json.asObject
                              usageJson <- obj("usage")
                              usageObj <- usageJson.asObject
                            yield usageObj

                            val promptTokens = usage
                              .flatMap(_("prompt_tokens"))
                              .flatMap(_.asNumber)
                              .flatMap(_.toInt)
                              .getOrElse(-1)

                            val completionTokens = usage
                              .flatMap(_("completion_tokens"))
                              .flatMap(_.asNumber)
                              .flatMap(_.toInt)
                              .getOrElse(-1)

                            IO.pure( Right(TranslationResponse(content.trim, promptTokens, completionTokens)) )
                          case None =>
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

object OpenAIRepository:
  def apply(): OpenAIRepository = new OpenAIRepository()
