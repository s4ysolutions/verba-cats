package solutions.s4y.verba.cli

import scala.annotation.tailrec

object Args:
  def parseArgs(args: List[String]): Map[String, String] =
    nextOption(Map.empty[String, String], args)

  private def isSwitch(s: String): Boolean = s.startsWith("-")
  // TODO: args should be synced with TranslationRequest fields somehow
  @tailrec
  private def nextOption(
      map: Map[String, String],
      list: List[String]
  ): Map[String, String] =
    list match {
      case Nil                                => map
      case ("--from" | "-f") :: value :: tail =>
        nextOption(map ++ Map("from" -> value), tail)
      case ("--to" | "-t") :: value :: tail =>
        nextOption(map ++ Map("to" -> value), tail)
      case ("--mode" | "-m") :: value :: tail =>
        nextOption(map ++ Map("mode" -> value), tail)
      case ("--quality" | "-q") :: value :: tail =>
        nextOption(map ++ Map("quality" -> value), tail)
      case ("--provider" | "-p") :: value :: tail =>
        nextOption(map ++ Map("provider" -> value), tail)
      case ("--ipa" | "-i") :: tail =>
        nextOption(map ++ Map("ipa" -> "true"), tail)
      case ("--help" | "-h") :: tail =>
        nextOption(map ++ Map("help" -> "true"), tail)
      // Handle positional <text> (last non-switch arg)
      case string :: Nil if !isSwitch(string) =>
        nextOption(map ++ Map("text" -> string), Nil)
      case option :: tail =>
        println(s"Unknown option: $option")
        println(usage)
        sys.exit(1)
    }

  val usage: String =
    """
        Usage: verba [-f|--from <lang>] [-t|--to <lang>] [-m|--mode <mode>] [-q|--quality <quality>] [-p|--provider <provider>] [-i|--ipa] <text>

        Options:
          -f, --from <lang>         Source language (e.g., en, fr) (required)
          -t, --to <lang>           Target language (e.g., en, fr) (required)
          -m, --mode <mode>         Mode: translate, explain, auto (default: auto)
          -q, --quality <quality>   Quality: fast, optimal, thinking (default: optimal)
          -p, --provider <provider> Translation provider: openai, gemini (default: openai)
          -i, --ipa                 Include IPA transcription (default: false)

        <text>                 Text to translate (required)
      """.trim
