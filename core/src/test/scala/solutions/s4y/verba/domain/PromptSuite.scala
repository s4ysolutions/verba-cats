package solutions.s4y.verba.domain

import munit.FunSuite
import cats.syntax.all.*
import solutions.s4y.verba.domain.vo.{Prompt, TranslationRequest, TranslationMode}

class PromptSuite extends FunSuite {

  test("derive trims Excerpt From, quotes and soft-hyphen") {
    val raw = "\"Hello\u00AD Excerpt From should be removed\""

    val validated = TranslationRequest(
      sourceText = Some(raw),
      sourceLang = Some("eng"),
      targetLang = Some("fra"),
      mode = Some("auto"),
      provider = Some("openai"),
      quality = Some("optimal"),
      ipa = Some(false)
    )

    validated match
      case Right(req) =>
        val p = req.prompt.value
        assert(p.contains("Hello"), "Prompt should contain Hello")
        assert(!p.contains("Excerpt From"), "Prompt should not contain 'Excerpt From'")
      case Left(err) =>
        fail(s"request validation failed: ${err.message}")
  }

  test("derive with Auto selects ExplainWords for short text") {
    val text = "Hi a"
    val validated = TranslationRequest(
      sourceText = Some(text),
      sourceLang = Some("eng"),
      targetLang = Some("fra"),
      mode = Some("auto"),
      provider = Some("openai"),
      quality = Some("optimal"),
      ipa = Some(false)
    )

    validated match
      case Right(req) =>
        val p = req.prompt.value
        assert(p.startsWith("Explain in") || p.contains("Explain the meaning"), "Should use ExplainWords mode")
      case Left(err) =>
        fail(s"request validation failed: ${err.message}")
  }

  test("derive with Auto selects TranslateSentence for longer text") {
    val text = "one two three"
    val validated = TranslationRequest(
      sourceText = Some(text),
      sourceLang = Some("eng"),
      targetLang = Some("fra"),
      mode = Some("auto"),
      provider = Some("openai"),
      quality = Some("optimal"),
      ipa = Some(false)
    )

    validated match
      case Right(req) =>
        val p = req.prompt.value
        assert(p.startsWith("Translate"), "Should use TranslateSentence mode")
      case Left(err) =>
        fail(s"request validation failed: ${err.message}")
  }

}

