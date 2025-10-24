package solutions.s4y.verba.domain

import munit.FunSuite
import cats.data.Validated
import cats.syntax.all.*
import solutions.s4y.verba.domain.vo.{Prompt, TranslationRequest, TranslationMode}

class PromptSuite extends FunSuite {

  test("derive trims Excerpt From, quotes and soft-hyphen") {
    val raw = "\"Hello\u00AD Excerpt From should be removed\""

    val validated = TranslationRequest(
      sourceText = raw,
      sourceLang = "eng",
      targetLang = "fra",
      mode = "auto",
      provider = "openai",
      quality = "optimal"
    )

    validated match
      case Validated.Valid(req) =>
        val p = Prompt.derive(req)
        assert(p.contains("Hello"))
        assert(!p.contains("Excerpt From"))
      case Validated.Invalid(errs) =>
        fail(s"request validation failed: ${errs.toList.map(_.message).mkString(",")}")
  }

  test("derive with Auto selects ExplainWords for short text") {
    val text = "Hi a"
    val validated = TranslationRequest(
      sourceText = text,
      sourceLang = "eng",
      targetLang = "fra",
      mode = "auto",
      provider = "openai",
      quality = "optimal"
    )

    validated match
      case Validated.Valid(req) =>
        val p = Prompt.derive(req)
        assert(p.startsWith("Explain in"))
      case Validated.Invalid(errs) =>
        fail(s"request validation failed: ${errs.toList.map(_.message).mkString(",")}")
  }

  test("derive with Auto selects TranslateSentence for longer text") {
    val text = "one two three"
    val validated = TranslationRequest(
      sourceText = text,
      sourceLang = "eng",
      targetLang = "fra",
      mode = "auto",
      provider = "openai",
      quality = "optimal"
    )

    validated match
      case Validated.Valid(req) =>
        val p = Prompt.derive(req)
        assert(p.startsWith("Translation only"))
      case Validated.Invalid(errs) =>
        fail(s"request validation failed: ${errs.toList.map(_.message).mkString(",")}")
  }

}

