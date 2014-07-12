package tetriz

import scala.language.postfixOps
import scala.annotation.tailrec
import scala.util.Random

import scalaz.{ @@, Tag, Lens, Endo, State, Monad }
import scalaz.std.vector._
import scalaz.syntax.monad._
import scalaz.syntax.foldable._

case class Point(x: Int, y: Int) {

  def swap = Point(y, x)

  def +(point: Point) = Point(x + point.x, y + point.y)

  def *(point: Point) = Point(x * point.x, y * point.y)

}

object Point {

  def x: Lens[Point, Int] =
    Lens.lensu((p, x) => p.copy(x = x), _.x)

  def y: Lens[Point, Int] =
    Lens.lensu((p, y) => p.copy(y = y), _.y)

}

object Piece {

  trait Tag

  type Type = Vector[Point] @@ Piece.Tag

  def apply(a: Point, b: Point, c: Point, d: Point): Piece.Type =
    Tag(Vector(a, b, c, d))

  def rotate(r: Int)(piece: Piece.Type): Piece.Type =
    Tag(piece.map(_.swap * Point(r, -r)))

}

object Bit {

  trait Tag

  type Type = Boolean @@ Tag

  def _0: Bit.Type = Tag[Boolean, Bit.Tag](false)

  def _1: Bit.Type = Tag[Boolean, Bit.Tag](true)

}

object Line {

  trait Tag

  type Type = Vector[Bit.Type] @@ Line.Tag

  def apply(length: Int): Line.Type =
    Tag(Vector.fill(length)(Bit._0))

  def bit(n: Int): Lens[Line.Type, Bit.Type] =
    Lens.lensu((l, b) => Tag(l.updated(n, b)), _(n))

}

object Field {

  trait Tag

  type Type = Vector[Line.Type] @@ Field.Tag

  def apply(width: Int, height: Int): Field.Type =
    Tag(Vector.fill(height)(Line(width)))

  def line(n: Int): Lens[Field.Type, Line.Type] =
    Lens.lensu((f, l) => Tag(f.updated(n, l)), _(n))

  def fix(piece: Piece.Type, position: Point): Endo[Field.Type] =
    piece.map(_ + position).foldMap(p => Endo(Field.line(p.y) >=> Line.bit(p.x) := Bit._1 exec))

  def check(field: Field.Type): Field.Type =
    Tag(Tetriz.empty ++ field.filterNot(_.forall(identity)) takeRight Tetriz.height)

  def overlaps(piece: Piece.Type, position: Point)(field: Field.Type): Boolean =
    piece.map(_ + position).exists(p => field.lift(p.y).flatMap(_.lift(p.x)).getOrElse(true))

}

case class Stage(field: Field.Type, piece: Piece.Type, position: Point)

object Stage {

  def field: Lens[Stage, Field.Type] =
    Lens.lensu((s, f) => s.copy(field = f), _.field)

  def piece: Lens[Stage, Piece.Type] =
    Lens.lensu((s, p) => s.copy(piece = p), _.piece)

  def position: Lens[Stage, Point] =
    Lens.lensu((s, p) => s.copy(position = p), _.position)

}

object Tetriz {

  def width = 10
  def height = 20

  def empty = Field(width, height)

  def start = Point(width / 2, 0)

  def I = Piece(Point(-1, 0), Point(0, 0), Point(1, 0), Point(2, 0))
  def O = Piece(Point(-1, 0), Point(0, 0), Point(-1, 1), Point(0, 1))
  def Z = Piece(Point(-1, 0), Point(0, 0), Point(0, 1), Point(1, 1))
  def S = Piece(Point(-1, 1), Point(0, 0), Point(0, 1), Point(1, 0))
  def J = Piece(Point(-1, 0), Point(-1, 1), Point(0, 1), Point(1, 1))
  def L = Piece(Point(-1, 1), Point(0, 1), Point(1, 0), Point(1, 1))
  def T = Piece(Point(-1, 1), Point(0, 0), Point(0, 1), Point(1, 1))

  def pieces = Vector(I, O, Z, S, J, L, T)

  def newPiece = pieces(Random.nextInt(pieces.size))

  def newStage = Stage(empty, newPiece, start)

  def move(n: Int)(stage: Stage) = {
    val position = stage.position + Point(n, 0)
    if (Field.overlaps(stage.piece, position)(stage.field))
      State.state(())
    else
      Stage.position := position
  }

  def run[A](x: Int, y: Int, r: Int): State[Stage, Unit] =
    for {
      field <- Stage.field
      piece <- Stage.piece
      position <- Stage.position
      _ <- if (r != 0 && !Field.overlaps(Piece.rotate(r)(piece), position)(field))
        Stage.piece %= Piece.rotate(r)
      else
        State.state[Stage, Unit](())
      position <- Stage.position
      _ <- if (!Field.overlaps(piece, (Point.x += x) exec position)(field))
        Stage.position >=> Point.x += x
      else
        State.state[Stage, Unit](())
      position <- Stage.position
      _ <- if (!Field.overlaps(piece, (Point.y += y) exec position)(field))
        Stage.position >=> Point.y += y
      else
        for {
          _ <- Stage.field %= Field.fix(piece, position).run
          _ <- Stage.piece := Tetriz.newPiece
          _ <- Stage.position := start
        } yield ()
      _ <- Stage.field %= Field.check
    } yield ()

}
