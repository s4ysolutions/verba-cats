package solutions.s4y.verba.domain.vo

import solutions.s4y.verba.domain.vo.TranslationMode.Auto

case class Prompt(value: String)

object Prompt:

  private object Sanity:
    private val extraTrimChars: Set[Char] =
      Set('"', '«', '»', '‘', '’', '“', '”', '.', ',', ';', ':')

    def clean(s: String): String =
      if s == null then ""
      else
        val withoutSoftHyphen = s.replace("\u00AD", "")

        // drop leading chars that are whitespace/newline or in extraTrimChars
        def trimBothEnds(str: String): String =
          val leftTrimmed =
            str.dropWhile(c => c.isWhitespace || extraTrimChars.contains(c))
          leftTrimmed.reverse
            .dropWhile(c => c.isWhitespace || extraTrimChars.contains(c))
            .reverse

        trimBothEnds(withoutSoftHyphen)

  def apply(
      raw: String,
      mode: TranslationMode,
      sourceLang: String,
      targetLang: String
  ): Prompt =

    val cleanedText =
      // Apple book annoyingly adds "Excerpt From" at the end of copied text
      val idx = raw.indexOf("Excerpt From")
      val before = if idx >= 0 then raw.substring(0, idx) else raw
      Sanity
        .clean(before)
        // normalize internal runs of whitespace to single spaces for nicer prompts
        .replaceAll("\\s+", " ")
        .trim

    val wordCount =
      if cleanedText.isEmpty then 0
      else cleanedText.split("\\s+").count(_.length > 2)

    val modeActual: TranslationMode =
      if mode == TranslationMode.Auto then
        if wordCount > 2 then TranslationMode.TranslateSentence
        else TranslationMode.ExplainWords
      else mode

    // noinspection NotImplementedCode
    val prompt = modeActual match
      case TranslationMode.TranslateSentence =>
        s"Translate from $sourceLang to $targetLang and provide IPA of $sourceLang. ONLY provide the translation and transcription. Do not include any introductory, conversational, or descriptive text.\n\n$cleanedText"
      case TranslationMode.ExplainWords =>
        s"Explain thoroughly in $targetLang the meaning of the following $sourceLang words and provide IPA of $sourceLang. ONLY provide the meaning and transcription. Do not include any introductory, conversational, or descriptive text.\n\n$cleanedText"
      case Auto => ???

    Prompt(prompt)
