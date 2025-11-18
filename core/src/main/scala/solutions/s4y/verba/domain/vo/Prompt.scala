package solutions.s4y.verba.domain.vo

import solutions.s4y.verba.domain.vo.TranslationMode.Auto

case class Prompt(value: String)

object Prompt:

  private object Sanity:
    private val extraTrimChars: Set[Char] =
      Set('"', '«', '»', '‘', '’', '“', '”', '.', ',', ';', ':')

    def clean(s: String): String =
      val withoutSoftHyphen = s.replace("\u00AD", "")

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
            val idx = r.indexOf("Excerpt From")
            if idx >= 0 then r.substring(0, idx) else r
          }
          .getOrElse("")
      Sanity
        .clean(before)
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

      // --------------------------------------------------------------
      // TRANSLATION MODE
      // --------------------------------------------------------------
      case TranslationMode.TranslateSentence =>

        val translatePrompt =
          sourceLang match
            case Some(src) =>
              s"""Translate the text from $src to $targetLang."""
            case None =>
              s"""First, DETECT the source language of the text.
Second, TRANSLATE it into $targetLang."""

        val ipaPrompt =
          if ipa then
            sourceLang match
              case Some(src) =>
                s"""Then provide IPA ONLY for the SOURCE-LANGUAGE words that appear in the INPUT text ($src).
NEVER provide IPA for the translation."""
              case None =>
                s"""Then provide IPA ONLY for the SOURCE-LANGUAGE words (as detected in step 1).
NEVER provide IPA for the translation."""
          else s"""Do NOT provide IPA."""

        val formatPrompt =
          if ipa then s"""Output format (STRICT):
<IPA of source-language words>
<Translation>"""
          else s"""Output ONLY the translation."""

        val limitPrompt =
          s"""Do not include explanations, comments, or additional text."""

        s"$translatePrompt\n$ipaPrompt\n$formatPrompt\n$limitPrompt\n\n$cleanedText"

      // --------------------------------------------------------------
      // EXPLAIN MODE (one or two words)
      // --------------------------------------------------------------
      case TranslationMode.ExplainWords =>
        if ipa then
          sourceLang match
            case Some(src) =>
              s"""
Explain the meaning of the following $src word(s) in $targetLang, as in a dictionary article.
Then give IPA ONLY for the SOURCE-LANGUAGE word(s).
Do NOT give IPA for the explanation in $targetLang.

Output format (STRICT):
<IPA of source word(s)>
<Explanation in $targetLang>

Do not include any introductory, conversational, or descriptive text.

$cleanedText
"""
            case None =>
              s"""
First, DETECT the language of the following word(s).
Second, explain their meaning in $targetLang, as in a dictionary article.
Then give IPA ONLY for the ORIGINAL SOURCE-LANGUAGE word(s).
Do NOT give IPA for the explanation in $targetLang.

Output format (STRICT):
<IPA of source word(s)>
<Explanation>

Do not include any introductory, conversational, or descriptive text.

$cleanedText
"""
        else
          sourceLang match
            case Some(src) =>
              s"""
Explain the meaning of the following $src word(s) in $targetLang, as in a dictionary article.
Provide ONLY the explanation. No IPA.

Do not include any introductory, conversational, or descriptive text.

$cleanedText
"""
            case None =>
              s"""
First, DETECT the language of the following word(s).
Second, explain their meaning in $targetLang, as in a dictionary article.
Provide ONLY the explanation. No IPA.

Do not include any introductory, conversational, or descriptive text.

$cleanedText
"""

      case Auto =>
        "ERROR: Auto mode should have been resolved earlier."

    Prompt(prompt)
