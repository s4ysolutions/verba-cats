package solutions.s4y.verba.http

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.comcast.ip4s.{Ipv4Address, Port}

import scala.annotation.tailrec

final case class ServerConfig(
    host: Ipv4Address,
    port: Port
)

object ServerConfig {
  private type ValidationResult[A] = ValidatedNec[String, A]

  private val defaultHost = "0.0.0.0"
  private val defaultPort = 8080

  def fromArgs(args: List[String]): Either[String, ServerConfig] = {
    val (hostOpt, portOpt, parseErrors) = parseArgs(args, None, None, List.empty)

    val hostStr = hostOpt.getOrElse(defaultHost)
    val portInt = portOpt.getOrElse(defaultPort)

    val vHost: ValidationResult[Ipv4Address] =
      Ipv4Address.fromString(hostStr) match {
        case Some(host) => host.validNec
        case None => s"Invalid IPv4 address: $hostStr".invalidNec
      }

    val vPort: ValidationResult[Port] =
      Port.fromInt(portInt) match {
        case Some(port) => port.validNec
        case None => s"Invalid port: $portInt (must be 1-65535)".invalidNec
      }

    val vParseErrors: ValidationResult[Unit] =
      if (parseErrors.isEmpty) ().validNec
      else parseErrors.mkString("\n").invalidNec

    val validationResult: ValidationResult[ServerConfig] =
      (vParseErrors, vHost, vPort).mapN { (_, h, p) =>
        ServerConfig(host = h, port = p)
      }

    validationResult.toEither.leftMap { errors =>
      s"Configuration errors:\n${errors.toList.mkString("\n")}\n\nUsage: --host <host> --port <port>"
    }
  }

  @tailrec
  private def parseArgs(
      remaining: List[String],
      hostOpt: Option[String],
      portOpt: Option[Int],
      errors: List[String]
  ): (Option[String], Option[Int], List[String]) = {
    remaining match {
      case Nil => (hostOpt, portOpt, errors)
      case "--host" :: host :: tail =>
        parseArgs(tail, Some(host), portOpt, errors)
      case "--host" :: Nil =>
        parseArgs(Nil, hostOpt, portOpt, errors :+ "Missing value for --host")
      case "--port" :: portStr :: tail =>
        portStr.toIntOption match {
          case Some(p) if p > 0 && p <= 65535 =>
            parseArgs(tail, hostOpt, Some(p), errors)
          case _ =>
            parseArgs(
              tail,
              hostOpt,
              portOpt,
              errors :+ s"Invalid port: $portStr (must be 1-65535)"
            )
        }
      case "--port" :: Nil =>
        parseArgs(Nil, hostOpt, portOpt, errors :+ "Missing value for --port")
      case unknown :: tail =>
        parseArgs(
          tail,
          hostOpt,
          portOpt,
          errors :+ s"Unknown argument: $unknown"
        )
    }
  }

}
