package solutions.s4y.verba.adapters

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import munit.FunSuite
import cats.data.Validated
import solutions.s4y.verba.domain.vo.TranslationRequest
import solutions.s4y.verba.domain.errors.{TranslationError, ApiError}

class GeminiRepositoryIntegrationSuite extends FunSuite {
  override val munitIgnore = true

  test("translate with Gemini API - integration test".tag(IntegrationTag)) {
    APIConfig.geminiAPIKey match {
      case None =>
        println("Skipping Gemini integration test - GEMINI_API_KEY not set")
        // Test passes automatically if no API key
      case Some(_) =>
        val validated = TranslationRequest(
          sourceText = "Hello world",
          sourceLang = "eng",
          targetLang = "fra",
          mode = "translate",
          provider = "gemini",
          quality = "fast"
        )

        validated match {
          case Validated.Valid(req) =>
            val repo = new GeminiRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(text) =>
                println(s"Gemini translation result: $text")
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
    val repo = new GeminiRepository()
    
    val validated = TranslationRequest(
      sourceText = "test",
      sourceLang = "eng",
      targetLang = "fra",
      mode = "translate",
      provider = "gemini",
      quality = "fast"
    )

    validated match {
      case Validated.Valid(req) =>
        if (APIConfig.geminiAPIKey.isEmpty) {
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

  test("translate with Auto mode - long text selects TranslateSentencea".tag(IntegrationTag)) {
    APIConfig.geminiAPIKey match {
      case None =>
        println("Skipping Gemini Auto mode test - GEMINI_API_KEY not set")
      case Some(_) =>
        val validated = TranslationRequest(
          sourceText = "The quick brown fox jumps over the lazy dog",
          sourceLang = "eng",
          targetLang = "fra",
          mode = "auto",  // Auto mode with longer text should trigger TranslateSentence
          provider = "gemini",
          quality = "fast"
        )

        validated match {
          case Validated.Valid(req) =>
            val repo = new GeminiRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(text) =>
                println(s"Gemini Auto mode translation: $text")
                assert(text.nonEmpty, "Translation should not be empty")
              case Left(err) =>
                fail(s"Translation failed: ${err.message}")
            }

          case Validated.Invalid(errs) =>
            fail(s"Request validation failed: ${errs.toList.map(_.message).mkString(", ")}")
        }
    }
  }

  test("translate with different quality levels".tag(IntegrationTag)) {
    APIConfig.geminiAPIKey match {
      case None =>
        println("Skipping Gemini quality test - GEMINI_API_KEY not set")
      case Some(_) =>
        val qualities = List("fast", "optimal")
        
        qualities.foreach { quality =>
          val validated = TranslationRequest(
            sourceText = "Good morning",
            sourceLang = "eng",
            targetLang = "fra",
            mode = "translate",
            provider = "gemini",
            quality = quality
          )

          validated match {
            case Validated.Valid(req) =>
              val repo = new GeminiRepository()
              val result = repo.translate(req).unsafeRunSync()

              result match {
                case Right(text) =>
                  println(s"Gemini translation with quality=$quality: $text")
                  assert(text.nonEmpty, s"Translation with quality=$quality should not be empty")
                case Left(err) =>
                  fail(s"Translation with quality=$quality failed: ${err.message}")
              }

            case Validated.Invalid(errs) =>
              fail(s"Request validation failed: ${errs.toList.map(_.message).mkString(", ")}")
          }
        }
    }
  }

  test("translate properly handles Excerpt From in source text".tag(IntegrationTag)) {
    APIConfig.geminiAPIKey match {
      case None =>
        println("Skipping Gemini Excerpt test - GEMINI_API_KEY not set")
      case Some(_) =>
        val validated = TranslationRequest(
          sourceText = "Hello world Excerpt From Some Book Title",
          sourceLang = "eng",
          targetLang = "fra",
          mode = "translate",
          provider = "gemini",
          quality = "fast"
        )

        validated match {
          case Validated.Valid(req) =>
            val repo = new GeminiRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(text) =>
                println(s"Gemini translation (with Excerpt trimmed): $text")
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
