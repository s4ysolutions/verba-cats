package solutions.s4y.verba.http.middleware

import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.headers.Authorization
import scribe.cats.LoggerExtras
import scribe.{Logger, Scribe}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

object AuthMiddleware:

  def apply(secret: String)(routes: HttpRoutes[IO]): HttpRoutes[IO] = Kleisli {
    (req: Request[IO]) =>
      logger.debug(
        "Authenticating request to " + s"${req.method} ${req.uri.path}"
      )

      if isLocal(req) then
        logger.debug("Request is from local address, skipping authentication")
        routes(req)
      else {
        val authHeader =
          req.headers.get[Authorization].map(_.credentials.renderString)

        authHeader match {
          case Some(headerValue) if checkWsse(headerValue, secret) =>
            routes(req)
          case Some(_) =>
            OptionT.liftF(
              loggerIO.warn(
                s"Unauthorized request to ${req.method} ${req.uri.path}"
              ) *>
                Forbidden("Invalid authentication token")
            )
          case None =>
            OptionT.liftF(
              loggerIO.warn(
                s"Missing authentication header for ${req.method} ${req.uri.path}"
              ) *>
                Forbidden("Missing authentication header")
            )
        }
      }
  }
  end apply

  private def isLocal(req: Request[IO]): Boolean =
    req.remote.exists { sa =>
      val s = sa.toString
      s.contains("127.0.0.1") || s.contains("::1") || s.contains("localhost")
    }

  // Purge nonces older than 20 seconds (double the time window for safety)
  private def purgeOldNonces(now: Long): Unit =
    val threshold = now - 20
    usedNonces.asScala.foreach { case (nonce, timestamp) =>
      if timestamp < threshold then usedNonces.remove(nonce)
    }

  // Check if nonce has been used and mark it as used
  private def isNonceReused(nonce: String, timestamp: Long): Boolean =
    purgeOldNonces(timestamp)
    val previousTimestamp: Long = usedNonces.putIfAbsent(nonce, timestamp)
    previousTimestamp != 0 // Long null deboxes to 0
  end isNonceReused

  private def checkWsse(authHeader: String, secret: String): Boolean =
    // WSSE format: UsernameToken Username="user", PasswordDigest="digest", Nonce="nonce", Created="timestamp"
    val wssePattern =
      """UsernameToken Username="([^"]+)",?\s*PasswordDigest="([^"]+)",?\s*Nonce="([^"]+)",?\s*Created="([^"]+)"""".r

    authHeader match {
      case wssePattern(username, digest, nonce, created) =>
        logger.debug(
          "Parsed WSSE header successfully: " +
            s"username=$username, nonce=$nonce, created=$created"
        )
        try {
          // Parse the created timestamp
          val createdInstant = Instant.parse(created)
          val now = Instant.now()
          val timeDiff = now.getEpochSecond - createdInstant.getEpochSecond

          // Check if timestamp is within acceptable window (-10 to +10 seconds)
          if (timeDiff < -10 || timeDiff > 10) {
            logger.warn(
              s"Timestamp outside acceptable range: $created (diff: $timeDiff seconds)"
            )
            return false
          }

          // Check if nonce has been reused
          if (isNonceReused(nonce, now.getEpochSecond)) {
            logger.warn(s"Nonce has been reused: $nonce")
            return false
          }

          // Calculate expected digest: Base64(SHA-256(Nonce + Created + Secret))
          val expectedDigest = hasher.sha256Base64(nonce + created + secret)
          /*Base64.getEncoder.encodeToString(
            MessageDigest
              .getInstance("SHA-256")
              .digest((nonce + created + secret).getBytes("UTF-8"))
          )*/
          // Verify digest matches
          digest == expectedDigest
        } catch {
          case _: Exception =>
            logger.error(s"Error parsing WSSE header: $authHeader")
            false
        }
      case _ =>
        logger.error(s"Invalid WSSE header format: $authHeader")
        false
    }
  end checkWsse

  private val hasher = HashUtil()

  // Map to track used nonces with their timestamps
  private val usedNonces: ConcurrentHashMap[String, Long] =
    new ConcurrentHashMap()

  private val logger: Logger = scribe.Logger("verba.http.auth")
  private val loggerIO: Scribe[IO] = logger.f[IO]
end AuthMiddleware
