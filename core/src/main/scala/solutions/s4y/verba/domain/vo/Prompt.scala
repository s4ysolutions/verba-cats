package solutions.s4y.verba.domain.vo

import solutions.s4y.verba.domain.vo.TranslationMode.Auto

object Prompt:

  private object Sanity:
    private val extraTrimChars: Set[Char] = Set('"', '«', '»')

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

  def derive(from: TranslationRequest): String =
    val raw = from.sourceText

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

    val mode: TranslationMode =
      if from.mode == TranslationMode.Auto then
        if wordCount > 2 then TranslationMode.TranslateSentence
        else TranslationMode.ExplainWords
      else from.mode

    //noinspection NotImplementedCode
    val prompt = mode match
      case TranslationMode.TranslateSentence =>
        s"Translation only (no extra) ${from.sourceLang} to ${from.targetLang}: $cleanedText"
      case TranslationMode.ExplainWords =>
        s"Explain in ${from.targetLang} language (only explanations no extra) the words of ${from.sourceLang}: $cleanedText"
      case Auto => ???

    prompt
