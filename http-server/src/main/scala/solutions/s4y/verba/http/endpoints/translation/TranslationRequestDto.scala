package solutions.s4y.verba.http.endpoints.translation

case class TranslationRequestDto(
    text: String,
    from: String,
    to: String,
    mode: Option[String] = None,
    provider: Option[String] = None,
    quality: Option[String] = None,
    ipa: Option[Boolean] = None
)
