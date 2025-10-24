package solutions.s4y.verba.ports.driven

import cats.effect.IO
import solutions.s4y.verba.domain.errors.TranslationError
import solutions.s4y.verba.domain.vo.TranslationRequest

trait TranslationRepository:
  def translate(
      request: TranslationRequest
  ): IO[Either[TranslationError, String]]
