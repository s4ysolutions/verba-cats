package solutions.s4y.verba.http.endpoints.translation

case class TranslationRequestDto(
    text: String,
    from: Option[String] = None,
    to: String,
    mode: Option[String] = None,
    provider: Option[String] = None,
    quality: Option[String] = None,
    ipa: Option[Boolean] = None
) {
  override def toString: String =
    s"TranslationRequestDto(text=${text.substring(0, math.min(text.length, 32))} from=$from, to=$to, mode=$mode , provider=$provider, quality=$quality, ipa=$ipa)"
}
