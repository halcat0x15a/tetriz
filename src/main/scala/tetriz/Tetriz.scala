package tetriz

import scala.language.postfixOps
import scala.annotation.tailrec

import scalaz.{ @@, Tag, Lens, Endo, State, Monad }
import scalaz.std.vector._
import scalaz.syntax.monad._
import scalaz.syntax.foldable._

case class Tetriz(field: Field, piece: Piece, position: Point)

object Tetriz {

  def field: Lens[Tetriz, Field] =
    Lens.lensu((s, f) => s.copy(field = f), _.field)

  def piece: Lens[Tetriz, Piece] =
    Lens.lensu((s, p) => s.copy(piece = p), _.piece)

  def position: Lens[Tetriz, Point] =
    Lens.lensu((s, p) => s.copy(position = p), _.position)

  def apply(): Tetriz =
    Tetriz(Field.empty, Piece(), Field.start)

  def run[A](x: Int, y: Int, r: Int): State[Tetriz, Unit] =
    for {
      field <- Tetriz.field
      piece <- Tetriz.piece
      position <- Tetriz.position
      _ <- if (r != 0 && !Field.overlaps(Piece.rotate(r)(piece), position)(field))
        Tetriz.piece %= Piece.rotate(r)
      else
        State.state[Tetriz, Unit](())
      piece <- Tetriz.piece
      position <- Tetriz.position
      _ <- if (!Field.overlaps(piece, (Point.x += x) exec position)(field))
        Tetriz.position >=> Point.x += x
      else
        State.state[Tetriz, Unit](())
      position <- Tetriz.position
      _ <- if (!Field.overlaps(piece, (Point.y += y) exec position)(field))
        Tetriz.position >=> Point.y += y
      else
        for {
          _ <- Tetriz.field %= Field.fix(piece, position).run
          _ <- Tetriz.piece := Piece()
          _ <- Tetriz.position := Field.start
        } yield ()
      _ <- Tetriz.field %= Field.check
    } yield ()

}
