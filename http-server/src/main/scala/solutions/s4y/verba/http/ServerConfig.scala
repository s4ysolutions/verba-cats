package solutions.s4y.verba.http

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.comcast.ip4s.{Ipv4Address, Port}

import scala.annotation.tailrec

final case class ServerConfig(
    host: Ipv4Address,
    port: Port,
    secret: String
)

object ServerConfig {
  private type ValidationResult[A] = ValidatedNec[String, A]

  def fromArgs(args: List[String]): Either[String, ServerConfig] = {
    val result = parseArgs(
      args,
      Ipv4Address.fromBytes(0, 0, 0, 0).validNec,
      Port.fromInt(8080).get.validNec,
      "Secret is required".invalidNec,
      ().validNec
    )

    result.toEither.leftMap { errors =>
      s"Configuration errors:\n${errors.toList.mkString("\n")}\n\nUsage: --host <host> --port <port> --secret <secret>"
    }
  }

  @tailrec
  private def parseArgs(
      remaining: List[String],
      host: ValidationResult[Ipv4Address],
      port: ValidationResult[Port],
      secret: ValidationResult[String],
      errors: ValidationResult[Unit]
  ): ValidationResult[ServerConfig] = {
    remaining match {
      case Nil =>
        (host, port, secret, errors).mapN { (h, p, s, _) =>
          ServerConfig(h, p, s)
        }
      case ("--host" | "-h") :: hostStr :: tail =>
        val newHost = Ipv4Address.fromString(hostStr) match {
          case Some(h) => h.validNec
          case None    => s"Invalid IPv4 address: $hostStr".invalidNec
        }
        parseArgs(tail, newHost, port, secret, errors)
      case ("--port" | "-p") :: portStr :: tail =>
        val newPort = Port.fromString(portStr) match {
          case Some(p) => p.validNec
          case None    => s"Invalid port: $portStr (must be 1-65535)".invalidNec
        }
        parseArgs(tail, host, newPort, secret, errors)
      case ("--secret" | "-s") :: secretStr :: tail =>
        parseArgs(tail, host, port, secretStr.validNec, errors)
      // missed value for required flag
      case flag :: Nil if isRequiredFlag(flag) =>
        val newError = s"Missing value for $flag".invalidNec[Unit]
        parseArgs(Nil, host, port, secret, errors.productR(newError))
      case unknown :: tail =>
        val newError = s"Unknown argument: $unknown".invalidNec[Unit]
        parseArgs(tail, host, port, secret, errors.productR(newError))
    }
  }

  private def isRequiredFlag(flag: String): Boolean =
    Set("--host", "-h", "--port", "-p", "--secret", "-s").contains(flag)
}
