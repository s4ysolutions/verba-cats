package solutions.s4y.verba.http

import cats.syntax.all.*
import com.comcast.ip4s.{Ipv4Address, Port}
import com.monovore.decline.*

final case class ServerConfig(
    host: Ipv4Address,
    port: Port,
    secret: String
)

object ServerConfig {
  private val hostOpt = Opts
    .option[String]("host", help = "Host address to bind to", short = "h")
    .mapValidated { hostStr =>
      Ipv4Address.fromString(hostStr) match {
        case Some(addr) => addr.valid
        case None       => s"Invalid IPv4 address: $hostStr".invalidNel
      }
    }
    .withDefault(Ipv4Address.fromBytes(0, 0, 0, 0))
    .orElse(
      Opts
        .env[String]("VERBA_HOST", help = "Host address from environment")
        .mapValidated { hostStr =>
          Ipv4Address.fromString(hostStr) match {
            case Some(addr) => addr.valid
            case None       => s"Invalid IPv4 address: $hostStr".invalidNel
          }
        }
    )

  private val portOpt = Opts
    .option[Int]("port", help = "Port to bind to", short = "p")
    .mapValidated { portInt =>
      Port.fromInt(portInt) match {
        case Some(p) => p.valid
        case None    => s"Invalid port: $portInt (must be 1-65535)".invalidNel
      }
    }
    .withDefault(Port.fromInt(8080).get)
    .orElse(
      Opts
        .env[Int]("VERBA_PORT", help = "Port from environment")
        .mapValidated { portInt =>
          Port.fromInt(portInt) match {
            case Some(p) => p.valid
            case None => s"Invalid port: $portInt (must be 1-65535)".invalidNel
          }
        }
    )

  private val secretOpt =
    Opts
      .option[String]("secret", help = "Authentication secret", short = "s")
      .orElse(
        Opts.env[String](
          "VERBA_SECRET",
          help = "Authentication secret from environment"
        )
      )

  private val configOpts =
    (hostOpt, portOpt, secretOpt).mapN(ServerConfig.apply)

  private val command = Command(
    name = "verba",
    header = "Verba HTTP Translation Server"
  )(configOpts)

  def fromArgs(args: List[String]): Either[String, ServerConfig] =
    command.parse(args, sys.env).leftMap(_.toString)
}
