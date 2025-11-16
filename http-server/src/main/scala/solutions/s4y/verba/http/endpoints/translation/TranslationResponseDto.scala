package solutions.s4y.verba.http.endpoints.translation

case class TranslationResponseDto(
    text: String,
    promptTokenCount: Int,
    textTokenCount: Int,
    time: Long
)
