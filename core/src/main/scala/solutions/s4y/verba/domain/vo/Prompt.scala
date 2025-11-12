package solutions.s4y.verba.domain.vo

import solutions.s4y.verba.domain.vo.TranslationMode.Auto

case class Prompt(value: String)

object Prompt:

  private object Sanity:
    private val extraTrimChars: Set[Char] =
      Set('"', '«', '»', '‘', '’', '“', '”', '.', ',', ';', ':')

    def clean(s: String): String =
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
      sourceLang: Option[String],
      targetLang: String,
      ipa: Boolean
  ): Prompt =

    val cleanedText =
      val before =
        Option(raw)
          .map { r =>
            // Apple book annoyingly adds "Excerpt From" at the end of copied text
            val idx = r.indexOf("Excerpt From")
            if idx >= 0 then r.substring(0, idx) else r
          }
          .getOrElse("")
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

    val prompt = modeActual match
      case TranslationMode.TranslateSentence =>
        if ipa then {
          val addon =
            s"and provide IPA of the translated word. ONLY provide the translation and transcription. Do not include any introductory, conversational, or descriptive text.\n\n$cleanedText"
          sourceLang match {
            case Some(srcLang) =>
              s"Translate from $srcLang to $targetLang $addon"
            case None =>
              s"Translate to $targetLang $addon"
          }
        } else {
          val addon =
            s"ONLY provide the translation. Do not include any introductory, conversational, or descriptive text.\n\n$cleanedText"
          sourceLang match {
            case Some(srcLang) =>
              s"Translate from $srcLang to $targetLang. $addon"
            case None =>
              s"Translate to $targetLang. $addon"
          }
        }
      case TranslationMode.ExplainWords =>
        if ipa then {
          val addon =
            s"using $targetLang and provide IPA of the explained word. ONLY provide the meaning and transcription. Do not include any introductory, conversational, or descriptive text.\n\n$cleanedText"
          sourceLang match {
            case Some(srcLang) =>
              s"Explain thoroughly, like a dictionary article, meaning of the following $srcLang words $addon"
            case None =>
              s"Explain thoroughly, like a dictionary article, meaning of the following words $addon"
          }
        } else {
          val addon =
            s"using $targetLang. ONLY provide the meaning. Do not include any introductory, conversational, or descriptive text.\n\n$cleanedText"
          sourceLang match {
            case Some(srcLang) =>
              s"Explain thoroughly, like a dictionary article, meaning of the following $srcLang words $addon"
            case None =>
              s"Explain thoroughly, like a dictionary article, meaning of the following words $addon."
          }
        }
      case Auto => ???

    Prompt(prompt)
