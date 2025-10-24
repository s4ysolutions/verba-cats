package solutions.s4y.verba.domain

import munit.FunSuite
import cats.data.Validated
import cats.syntax.all.*
import solutions.s4y.verba.domain.errors.RequestValidationError
import solutions.s4y.verba.domain.vo.{TranslationMode, TranslationProvider, TranslationQuality, TranslationRequest}

class TranslationRequestSuite extends FunSuite {

  test("apply returns valid request for valid inputs") {
    val res = TranslationRequest(
      sourceText = "Hello",
      sourceLang = "eng",
      targetLang = "fra",
      mode = "translate",
      provider = "openai",
      quality = "optimal"
    )

    res match {
      case Validated.Valid(req) =>
        assertEquals(req.sourceText, "Hello")
        assertEquals(req.sourceLang, "eng")
        assertEquals(req.targetLang, "fra")
        assertEquals(req.mode, TranslationMode.TranslateSentence)
        assertEquals(req.provider, TranslationProvider.OpenAI)
        assertEquals(req.quality, TranslationQuality.Optimal)

      case Validated.Invalid(errs) =>
        fail(s"Expected valid request but got errors: ${errs.toList.map(_.message).mkString(", ")}")
    }
  }

  test("apply accumulates multiple validation errors") {
    val res = TranslationRequest(
      sourceText = "",
      sourceLang = "e",       // too short
      targetLang = "x",       // too short
      mode = "badmode",       // invalid
      provider = "unknown",   // invalid
      quality = "badquality"  // invalid
    )

    res match {
      case Validated.Valid(_) =>
        fail("Expected invalid result due to multiple validation errors")

      case Validated.Invalid(errs) =>
        val list = errs.toList

        assert(list.exists {
          case RequestValidationError.EmptyString => true
          case _                                   => false
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
