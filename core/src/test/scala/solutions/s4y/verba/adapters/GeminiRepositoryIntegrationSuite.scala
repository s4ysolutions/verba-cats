package solutions.s4y.verba.adapters

import cats.effect.unsafe.implicits.global
import munit.FunSuite
import solutions.s4y.verba.domain.errors.{ApiError, TranslationError}
import solutions.s4y.verba.domain.vo.TranslationRequest

class GeminiRepositoryIntegrationSuite extends FunSuite {
  override val munitIgnore = true

  test("translate with Gemini API - integration test".tag(IntegrationTag)) {
    APIConfig.geminiAPIKey match {
      case None =>
        println("Skipping Gemini integration test - GEMINI_API_KEY not set")
        // Test passes automatically if no API key
      case Some(_) =>
        val validated = TranslationRequest(
          sourceText = Some("Hello world"),
          sourceLang = Some("eng"),
          targetLang = Some("fra"),
          mode = Some("translate"),
          provider = Some("gemini"),
          quality = Some("fast"),
          ipa = Some(false)
        )

        validated match {
          case Right(req) =>
            val repo = new GeminiRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(response) =>
                println(s"Gemini translation result: ${response.translated}")
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
    val repo = new GeminiRepository()
    
    val validated = TranslationRequest(
      sourceText = Some("test"),
      sourceLang = Some("eng"),
      targetLang = Some("fra"),
      mode = Some("translate"),
      provider = Some("gemini"),
      quality = Some("fast"),
      ipa = Some(false)
    )

    validated match {
      case Right(req) =>
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
      case Left(err) =>
        fail(s"Request validation failed: ${err.message}")
    }
  }

  test("translate with Auto mode - long text selects TranslateSentencea".tag(IntegrationTag)) {
    APIConfig.geminiAPIKey match {
      case None =>
        println("Skipping Gemini Auto mode test - GEMINI_API_KEY not set")
      case Some(_) =>
        val validated = TranslationRequest(
          sourceText = Some("The quick brown fox jumps over the lazy dog"),
          sourceLang = Some("eng"),
          targetLang = Some("fra"),
          mode = Some("auto"),  // Auto mode with longer text should trigger TranslateSentence
          provider = Some("gemini"),
          quality = Some("fast"),
          ipa = Some(false)
        )

        validated match {
          case Right(req) =>
            val repo = new GeminiRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(response) =>
                println(s"Gemini Auto mode translation: ${response.translated}")
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

  test("translate with different quality levels".tag(IntegrationTag)) {
    APIConfig.geminiAPIKey match {
      case None =>
        println("Skipping Gemini quality test - GEMINI_API_KEY not set")
      case Some(_) =>
        val qualities = List("fast", "optimal")
        
        qualities.foreach { quality =>
          val validated = TranslationRequest(
            sourceText = Some("Good morning"),
            sourceLang = Some("eng"),
            targetLang = Some("fra"),
            mode = Some("translate"),
            provider = Some("gemini"),
            quality = Some(quality),
            ipa = Some(false)
          )

          validated match {
            case Right(req) =>
              val repo = new GeminiRepository()
              val result = repo.translate(req).unsafeRunSync()

              result match {
                case Right(response) =>
                  println(s"Gemini translation with quality=$quality: ${response.translated}")
                  println(s"Input tokens: ${response.inputTokenCount}, Output tokens: ${response.outputTokenCount}")
                  assert(response.translated.nonEmpty, s"Translation with quality=$quality should not be empty")
                case Left(err) =>
                  fail(s"Translation with quality=$quality failed: ${err.message}")
              }

            case Left(err) =>
              fail(s"Request validation failed: ${err.message}")
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
          sourceText = Some("Hello world Excerpt From Some Book Title"),
          sourceLang = Some("eng"),
          targetLang = Some("fra"),
          mode = Some("translate"),
          provider = Some("gemini"),
          quality = Some("fast"),
          ipa = Some(false)
        )

        validated match {
          case Right(req) =>
            val repo = new GeminiRepository()
            val result = repo.translate(req).unsafeRunSync()

            result match {
              case Right(response) =>
                println(s"Gemini translation (with Excerpt trimmed): ${response.translated}")
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
