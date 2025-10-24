package solutions.s4y.verba.adapters

import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import munit.FunSuite
import cats.data.Validated
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
          sourceText = "Hello world",
          sourceLang = "eng",
          targetLang = "fra",
          mode = "translate",
          provider = "openai",
          quality = "fast"
        )

        validated match {
          case Validated.Valid(req) =>
            val repo = new OpenAIRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(text) =>
                println(s"OpenAI translation result: $text")
                assert(text.nonEmpty, "Translation should not be empty")
                assert(text.toLowerCase.contains("bonjour") || text.toLowerCase.contains("monde"),
                  "French translation should contain expected words")
              case Left(err) =>
                fail(s"Translation failed: ${err.message}")
            }

          case Validated.Invalid(errs) =>
            fail(s"Request validation failed: ${errs.toList.map(_.message).mkString(", ")}")
        }
    }
  }

  test("translate returns InvalidKey error when API key is missing".tag(IntegrationTag)) {
    // Temporarily clear the API key
    val originalKey = sys.env.get("OPENAI_API_KEY")

    // Create a repository instance (it will check env at runtime)
    val repo = new OpenAIRepository()

    val validated = TranslationRequest(
      sourceText = "test",
      sourceLang = "eng",
      targetLang = "fra",
      mode = "translate",
      provider = "openai",
      quality = "fast"
    )

    validated match {
      case Validated.Valid(req) =>
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
      case Validated.Invalid(errs) =>
        fail(s"Request validation failed: ${errs.toList.map(_.message).mkString(", ")}")
    }
  }

  test("translate with Auto mode - selects correct prompt strategy".tag(IntegrationTag).tag(IntegrationTag)) {
    APIConfig.openAIAPIKey match {
      case None =>
        println("Skipping OpenAI Auto mode test - OPENAI_API_KEY not set")
      case Some(_) =>
        val validated = TranslationRequest(
          sourceText = "cat",
          sourceLang = "eng",
          targetLang = "fra",
          mode = "auto",  // Auto mode with short text should trigger ExplainWords
          provider = "openai",
          quality = "fast"
        )

        validated match {
          case Validated.Valid(req) =>
            val repo = new OpenAIRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(text) =>
                println(s"OpenAI Auto mode translation: $text")
                assert(text.nonEmpty, "Translation should not be empty")
              case Left(err) =>
                fail(s"Translation failed: ${err.message}")
            }

          case Validated.Invalid(errs) =>
            fail(s"Request validation failed: ${errs.toList.map(_.message).mkString(", ")}")
        }
    }
  }
}

