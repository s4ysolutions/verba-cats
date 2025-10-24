package solutions.s4y.verba.scribe

import cats.effect.Sync
import org.typelevel.log4cats.SelfAwareStructuredLogger
import scribe.Logger

object ScribeLogger {

  def getLogger[F[_]: Sync]: SelfAwareStructuredLogger[F] =
    new ScribeLoggerInternal[F](scribe.Logger.root)

  def getLoggerFromScribe[F[_]: Sync](
      scribeLogger: Logger
  ): SelfAwareStructuredLogger[F] =
    new ScribeLoggerInternal[F](scribeLogger)

  private final class ScribeLoggerInternal[F[_]: Sync](logger: Logger)(implicit
      F: Sync[F]
  ) extends SelfAwareStructuredLogger[F] {

    override def isTraceEnabled: F[Boolean] = F.pure(true)
    override def isDebugEnabled: F[Boolean] = F.pure(true)
    override def isInfoEnabled: F[Boolean] = F.pure(true)
    override def isWarnEnabled: F[Boolean] = F.pure(true)
    override def isErrorEnabled: F[Boolean] = F.pure(true)

    override def trace(msg: => String): F[Unit] = F.delay(logger.trace(msg))
    override def trace(t: Throwable)(msg: => String): F[Unit] =
      F.delay(logger.trace(msg, t))
    override def trace(ctx: Map[String, String])(msg: => String): F[Unit] =
      F.delay(logger.trace(msg))
    override def trace(ctx: Map[String, String], t: Throwable)(
        msg: => String
    ): F[Unit] = F.delay(logger.trace(msg, t))

    override def debug(msg: => String): F[Unit] = F.delay(logger.debug(msg))
    override def debug(t: Throwable)(msg: => String): F[Unit] =
      F.delay(logger.debug(msg, t))
    override def debug(ctx: Map[String, String])(msg: => String): F[Unit] =
      F.delay(logger.debug(msg))
    override def debug(ctx: Map[String, String], t: Throwable)(
        msg: => String
    ): F[Unit] = F.delay(logger.debug(msg, t))

    override def info(msg: => String): F[Unit] = F.delay(logger.info(msg))
    override def info(t: Throwable)(msg: => String): F[Unit] =
      F.delay(logger.info(msg, t))
    override def info(ctx: Map[String, String])(msg: => String): F[Unit] =
      F.delay(logger.info(msg))
    override def info(ctx: Map[String, String], t: Throwable)(
        msg: => String
    ): F[Unit] = F.delay(logger.info(msg, t))

    override def warn(msg: => String): F[Unit] = F.delay(logger.warn(msg))
    override def warn(t: Throwable)(msg: => String): F[Unit] =
      F.delay(logger.warn(msg, t))
    override def warn(ctx: Map[String, String])(msg: => String): F[Unit] =
      F.delay(logger.warn(msg))
    override def warn(ctx: Map[String, String], t: Throwable)(
        msg: => String
    ): F[Unit] = F.delay(logger.warn(msg, t))

    override def error(msg: => String): F[Unit] = F.delay(logger.error(msg))
    override def error(t: Throwable)(msg: => String): F[Unit] =
      F.delay(logger.error(msg, t))
    override def error(ctx: Map[String, String])(msg: => String): F[Unit] =
      F.delay(logger.error(msg))
    override def error(ctx: Map[String, String], t: Throwable)(
        msg: => String
    ): F[Unit] = F.delay(logger.error(msg, t))
  }
}
