package solutions.s4y.verba.domain.errors

enum RequestValidationError:
  case EmptyString(field: String)
  case InvalidProvider(provider: String)
  case InvalidQuality(quality: String)
  case InvalidMode(mode: String)
  case LangTooShort(lang: String)
  case LangTooLong(lang: String)

  def message: String = this match
    case EmptyString(s)     => s"\"$s\" cannot be empty"
    case InvalidProvider(p) => s"Invalid provider: $p"
    case InvalidQuality(q)  => s"Invalid quality: $q"
    case InvalidMode(m)     => s"Invalid mode: $m"
    case LangTooShort(l)    => s"Lang too short: $l"
    case LangTooLong(l)     => s"Language too long: $l"
