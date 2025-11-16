package solutions.s4y.verba.domain.vo

final case class TranslationResponse(
    text: String,
    promptTokenCount: Int,
    textTokenCount: Int
)
