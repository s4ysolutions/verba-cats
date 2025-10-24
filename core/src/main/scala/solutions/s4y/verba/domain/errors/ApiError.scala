package solutions.s4y.verba.domain.errors

enum ApiError:
  case InvalidKey
  case RateLimitExceeded
  case EncodingFailed(data: String, err: Throwable)
  case DecodingFailed(data: String, err: Throwable)
  case Networking(err: Throwable)
  case TemporaryUnavailable
  case Unexpected(info: String)

  def message: String = this match
    case InvalidKey                => "Invalid API key"
    case RateLimitExceeded         => "Rate limit exceeded"
    case EncodingFailed(data, err) =>
      s"Failed to encode data: $data. Error: ${err.getMessage}"
    case DecodingFailed(data, err) =>
      s"Failed to decode data: $data. Error: ${err.getMessage}"
    case Networking(err)  => s"Networking error: ${err.getMessage}"
    case TemporaryUnavailable      => "Service is temporarily unavailable"
    case Unexpected(info) => s"Unexpected error: $info"

  def cause: Option[Throwable] = this match
    case EncodingFailed(_, err) => Some(err)
    case DecodingFailed(_, err) => Some(err)
    case Networking(err)        => Some(err)
    case _                      => None
