package solutions.s4y.verba.adapters

/**
 * Simple environment-backed API config.
 */
object APIConfig:
  def geminiAPIKey: Option[String] = sys.env.get("GEMINI_API_KEY")
  def openAIAPIKey: Option[String] = sys.env.get("OPENAI_API_KEY")

