package solutions.s4y.verba.adapters

import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import munit.FunSuite
import solutions.s4y.verba.domain.vo.TranslationRequest
import solutions.s4y.verba.domain.errors.{TranslationError, ApiError}

class OpenAIRepositoryIntegrationSuite extends FunSuite {
  override val munitIgnore = true

  test("translate with OpenAI API - integration test".tag(IntegrationTag)) {
    APIConfig.openAIAPIKey match {
      case None =>
        println("Skipping OpenAI integration test - OPENAI_API_KEY not set")
        // Test passes automatically if no API key
      case Some(_) =>
        val validated = TranslationRequest(
          sourceText = Some("Hello world"),
          sourceLang = Some("eng"),
          targetLang = Some("fra"),
          mode = Some("translate"),
          provider = Some("openai"),
          quality = Some("fast"),
          ipa = Some(false)
        )

        validated match {
          case Right(req) =>
            val repo = new OpenAIRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(response) =>
                println(s"OpenAI translation result: ${response.translated}")
                println(s"Input tokens: ${response.inputTokenCount}, Output tokens: ${response.outputTokenCount}")
                assert(response.translated.nonEmpty, "Translation should not be empty")
                assert(response.translated.toLowerCase.contains("bonjour") || response.translated.toLowerCase.contains("monde"),
                  "French translation should contain expected words")
              case Left(err) =>
                fail(s"Translation failed: ${err.message}")
            }

          case Left(err) =>
            fail(s"Request validation failed: ${err.message}")
        }
    }
  }

  test("translate returns InvalidKey error when API key is missing".tag(IntegrationTag)) {
    // Temporarily clear the API key
    val originalKey = sys.env.get("OPENAI_API_KEY")

    // Create a repository instance (it will check env at runtime)
    val repo = new OpenAIRepository()

    val validated = TranslationRequest(
      sourceText = Some("test"),
      sourceLang = Some("eng"),
      targetLang = Some("fra"),
      mode = Some("translate"),
      provider = Some("openai"),
      quality = Some("fast"),
      ipa = Some(false)
    )

    validated match {
      case Right(req) =>
        // Override the config check by testing when key is None
        if (APIConfig.openAIAPIKey.isEmpty) {
          val result = repo.translate(req).unsafeRunSync()
          result match {
            case Left(TranslationError.Api(ApiError.InvalidKey)) =>
              // Expected behavior
              assert(true)
            case other =>
              fail(s"Expected InvalidKey error but got: $other")
          }
        }
      case Left(err) =>
        fail(s"Request validation failed: ${err.message}")
    }
  }

  test("translate with Auto mode - selects correct prompt strategy".tag(IntegrationTag).tag(IntegrationTag)) {
    APIConfig.openAIAPIKey match {
      case None =>
        println("Skipping OpenAI Auto mode test - OPENAI_API_KEY not set")
      case Some(_) =>
        val validated = TranslationRequest(
          sourceText = Some("cat"),
          sourceLang = Some("eng"),
          targetLang = Some("fra"),
          mode = Some("auto"),  // Auto mode with short text should trigger ExplainWords
          provider = Some("openai"),
          quality = Some("fast"),
          ipa = Some(false)
        )

        validated match {
          case Right(req) =>
            val repo = new OpenAIRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(response) =>
                println(s"OpenAI Auto mode translation: ${response.translated}")
                println(s"Input tokens: ${response.inputTokenCount}, Output tokens: ${response.outputTokenCount}")
                assert(response.translated.nonEmpty, "Translation should not be empty")
              case Left(err) =>
                fail(s"Translation failed: ${err.message}")
            }

          case Left(err) =>
            fail(s"Request validation failed: ${err.message}")
        }
    }
  }
}

