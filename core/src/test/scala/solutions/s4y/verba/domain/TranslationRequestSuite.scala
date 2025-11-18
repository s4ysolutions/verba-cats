package solutions.s4y.verba.domain

import munit.FunSuite
import cats.syntax.all.*
import solutions.s4y.verba.domain.errors.RequestValidationError
import solutions.s4y.verba.domain.vo.{TranslationMode, TranslationProviders, TranslationQuality, TranslationRequest}

class TranslationRequestSuite extends FunSuite {

  test("apply returns valid request for valid inputs") {
    val res = TranslationRequest(
      sourceText = Some("Hello"),
      sourceLang = Some("eng"),
      targetLang = Some("fra"),
      mode = Some("translate"),
      provider = Some("openai"),
      quality = Some("optimal"),
      ipa = Some(false)
    )

    res match {
      case Right(req) =>
        assertEquals(req.sourceLang, Some("eng"))
        assertEquals(req.targetLang, "fra")
        assertEquals(req.mode, TranslationMode.TranslateSentence)
        assertEquals(req.provider, TranslationProviders.OpenAI)
        assertEquals(req.quality, TranslationQuality.Optimal)

      case Left(err) =>
        fail(s"Expected valid request but got errors: ${err.message}")
    }
  }

  test("apply accumulates multiple validation errors") {
    val res = TranslationRequest(
      sourceText = Some(""),
      sourceLang = Some("e"),       // too short
      targetLang = Some("x"),       // too short
      mode = Some("badmode"),       // invalid
      provider = Some("unknown"),   // invalid
      quality = Some("badquality"), // invalid
      ipa = Some(false)
    )

    res match {
      case Right(_) =>
        fail("Expected invalid result due to multiple validation errors")

      case Left(err) =>
        val list = err.errors.toList

        assert(list.exists {
          case RequestValidationError.EmptyString(_) => true
          case _                                      => false
        }, "expected EmptyString error")

        assert(list.exists {
          case RequestValidationError.LangTooShort(l) if l == "e" => true
          case _                                                  => false
        }, "expected LangTooShort for sourceLang 'e'")

        assert(list.exists {
          case RequestValidationError.LangTooShort(l) if l == "x" => true
          case _                                                  => false
        }, "expected LangTooShort for targetLang 'x'")

        assert(list.exists {
          case RequestValidationError.InvalidMode(m) if m == "badmode" => true
          case _                                                       => false
        }, "expected InvalidMode for 'badmode'")

        assert(list.exists {
          case RequestValidationError.InvalidProvider(p) if p == "unknown" => true
          case _                                                          => false
        }, "expected InvalidProvider for 'unknown'")

        assert(list.exists {
          case RequestValidationError.InvalidQuality(q) if q == "badquality" => true
          case _                                                              => false
        }, "expected InvalidQuality for 'badquality'")
    }
  }
}
