package solutions.s4y.verba.cli

import munit.FunSuite

class ArgsSuite extends FunSuite {
  
  test("parseArgs returns empty map when no args") {
    val args = List.empty[String]
    val m = Args.parseArgs(args)
    assertEquals(m, Map.empty[String, String])
  }

  test("parseArgs handles short flag -f") {
    val args = List("-f", "en")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
  }

  test("parseArgs handles long flag --from") {
    val args = List("--from", "en")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
  }

  test("parseArgs handles short flag -t") {
    val args = List("-t", "fr")
    val m = Args.parseArgs(args)
    assertEquals(m.get("to"), Some("fr"))
  }

  test("parseArgs handles long flag --to") {
    val args = List("--to", "fr")
    val m = Args.parseArgs(args)
    assertEquals(m.get("to"), Some("fr"))
  }

  test("parseArgs handles short flag -m") {
    val args = List("-m", "translate")
    val m = Args.parseArgs(args)
    assertEquals(m.get("mode"), Some("translate"))
  }

  test("parseArgs handles long flag --mode") {
    val args = List("--mode", "explain")
    val m = Args.parseArgs(args)
    assertEquals(m.get("mode"), Some("explain"))
  }

  test("parseArgs handles short flag -q") {
    val args = List("-q", "fast")
    val m = Args.parseArgs(args)
    assertEquals(m.get("quality"), Some("fast"))
  }

  test("parseArgs handles long flag --quality") {
    val args = List("--quality", "optimal")
    val m = Args.parseArgs(args)
    assertEquals(m.get("quality"), Some("optimal"))
  }

  test("parseArgs handles short flag -p") {
    val args = List("-p", "gemini")
    val m = Args.parseArgs(args)
    assertEquals(m.get("provider"), Some("gemini"))
  }

  test("parseArgs handles long flag --provider") {
    val args = List("--provider", "openai")
    val m = Args.parseArgs(args)
    assertEquals(m.get("provider"), Some("openai"))
  }

  test("parseArgs handles short flag -i for ipa") {
    val args = List("-i")
    val m = Args.parseArgs(args)
    assertEquals(m.get("ipa"), Some("true"))
  }

  test("parseArgs handles long flag --ipa") {
    val args = List("--ipa")
    val m = Args.parseArgs(args)
    assertEquals(m.get("ipa"), Some("true"))
  }

  test("parseArgs handles short flag -h for help") {
    val args = List("-h")
    val m = Args.parseArgs(args)
    assertEquals(m.get("help"), Some("true"))
  }

  test("parseArgs handles long flag --help") {
    val args = List("--help")
    val m = Args.parseArgs(args)
    assertEquals(m.get("help"), Some("true"))
  }

  test("parseArgs captures positional text when last non-switch") {
    val args = List("hello world")
    val m = Args.parseArgs(args)
    assertEquals(m.get("text"), Some("hello world"))
  }

  test("parseArgs captures positional text at the end") {
    val args = List("-f", "en", "hello")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("text"), Some("hello"))
  }

  test("parseArgs handles all short options with values") {
    val args = List("-f", "en", "-t", "fr", "-m", "translate", "-q", "optimal", "-p", "openai", "hello")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("to"), Some("fr"))
    assertEquals(m.get("mode"), Some("translate"))
    assertEquals(m.get("quality"), Some("optimal"))
    assertEquals(m.get("provider"), Some("openai"))
    assertEquals(m.get("text"), Some("hello"))
  }

  test("parseArgs handles all long options with values") {
    val args = List("--from", "en", "--to", "fr", "--mode", "explain", "--quality", "fast", "--provider", "gemini", "hello")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("to"), Some("fr"))
    assertEquals(m.get("mode"), Some("explain"))
    assertEquals(m.get("quality"), Some("fast"))
    assertEquals(m.get("provider"), Some("gemini"))
    assertEquals(m.get("text"), Some("hello"))
  }

  test("parseArgs handles mixed short and long options") {
    val args = List("-f", "en", "--to", "fr", "-m", "translate", "--quality", "optimal", "hello")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("to"), Some("fr"))
    assertEquals(m.get("mode"), Some("translate"))
    assertEquals(m.get("quality"), Some("optimal"))
    assertEquals(m.get("text"), Some("hello"))
  }

  test("parseArgs handles boolean flags with other options") {
    val args = List("-i", "-f", "en", "-t", "fr", "hello")
    val m = Args.parseArgs(args)
    assertEquals(m.get("ipa"), Some("true"))
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("to"), Some("fr"))
    assertEquals(m.get("text"), Some("hello"))
  }

  test("parseArgs handles multiple boolean flags") {
    val args = List("-i", "-h", "hello")
    val m = Args.parseArgs(args)
    assertEquals(m.get("ipa"), Some("true"))
    assertEquals(m.get("help"), Some("true"))
    assertEquals(m.get("text"), Some("hello"))
  }

  test("parseArgs handles options in any order") {
    val args = List("-i", "-q", "fast", "-p", "gemini", "-t", "de", "-f", "en", "text")
    val m = Args.parseArgs(args)
    assertEquals(m.get("ipa"), Some("true"))
    assertEquals(m.get("quality"), Some("fast"))
    assertEquals(m.get("provider"), Some("gemini"))
    assertEquals(m.get("to"), Some("de"))
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("text"), Some("text"))
  }

  test("parseArgs handles text with spaces") {
    val args = List("-f", "en", "-t", "fr", "hello world from scala")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("to"), Some("fr"))
    assertEquals(m.get("text"), Some("hello world from scala"))
  }

  test("parseArgs overrides duplicate options with last value") {
    val args = List("-f", "en", "-f", "de", "hello")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("de"))
    assertEquals(m.get("text"), Some("hello"))
  }

  test("parseArgs handles all options together") {
    val args = List(
      "-f", "en",
      "--to", "fr",
      "-m", "translate",
      "--quality", "thinking",
      "-p", "openai",
      "-i",
      "--help",
      "translate this text"
    )
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("to"), Some("fr"))
    assertEquals(m.get("mode"), Some("translate"))
    assertEquals(m.get("quality"), Some("thinking"))
    assertEquals(m.get("provider"), Some("openai"))
    assertEquals(m.get("ipa"), Some("true"))
    assertEquals(m.get("help"), Some("true"))
    assertEquals(m.get("text"), Some("translate this text"))
  }

  test("parseArgs handles just flags without text") {
    val args = List("-f", "en", "-t", "fr", "-i")
    val m = Args.parseArgs(args)
    assertEquals(m.get("from"), Some("en"))
    assertEquals(m.get("to"), Some("fr"))
    assertEquals(m.get("ipa"), Some("true"))
  }

  test("usage contains all options") {
    assert(Args.usage.contains("--from"))
    assert(Args.usage.contains("-f"))
    assert(Args.usage.contains("--to"))
    assert(Args.usage.contains("-t"))
    assert(Args.usage.contains("--mode"))
    assert(Args.usage.contains("-m"))
    assert(Args.usage.contains("--quality"))
    assert(Args.usage.contains("-q"))
    assert(Args.usage.contains("--provider"))
    assert(Args.usage.contains("-p"))
    assert(Args.usage.contains("--ipa"))
    assert(Args.usage.contains("-i"))
  }
}

