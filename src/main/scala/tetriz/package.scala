import scala.language.postfixOps
import scala.util.Random

import scalaz.{ @@, Monoid, Endo, Lens, PLens, Tag, Tags }
import scalaz.std.anyVal._
import scalaz.std.tuple._
import scalaz.std.vector._
import scalaz.syntax.id._
import scalaz.syntax.monoid._
import scalaz.syntax.monad._
import scalaz.syntax.foldable._
import scalaz.syntax.std.tuple._

package object tetriz {

  type Point = (Int, Int)

  object Point {

    def x: Lens[Point, Int] = Lens.firstLens

    def y: Lens[Point, Int] = Lens.secondLens

  }

  type Piece = Vector[Point]

  object Piece {

    def I = Vector((-1, 0), (0, 0), (1, 0), (2, 0))
    def O = Vector((-1, 0), (0, 0), (-1, 1), (0, 1))
    def Z = Vector((-1, 0), (0, 0), (0, 1), (1, 1))
    def S = Vector((-1, 1), (0, 0), (0, 1), (1, 0))
    def J = Vector((-1, 0), (-1, 1), (0, 1), (1, 1))
    def L = Vector((-1, 1), (0, 1), (1, 0), (1, 1))
    def T = Vector((-1, 1), (0, 0), (0, 1), (1, 1))

    def apply() = Random.shuffle(Vector(I, O, Z, S, J, L, T)).head
    
    def rotate(r: Int)(piece: Piece): Piece = {
      val inverse = for {
        x <- Point.x
        y <- Point.y
        _ <- Point.x := y * r
        _ <- Point.y := x * -r
      } yield ()
      piece.map(inverse.exec)
    }

  }

  type Field = Vector[Vector[Boolean]]

  object Field {

    def width = 10
    def height = 20

    def empty = Vector.fill(height)(Vector.fill(width)(false))

    def start = (width / 2, 0)

    def bit(x: Int, y: Int): PLens[Field, Boolean] =
      PLens.vectorNthPLens(y) >=> PLens.vectorNthPLens(x)

    def fix(piece: Piece, position: Point): Endo[Field] =
      piece.map(_ |+| position).foldMap(p => Endo(bit(Point.x.get(p), Point.y.get(p)) := true exec))

    def check(field: Field): Field =
      empty ++ field.filterNot(_.forall(identity)) takeRight height

    def overlaps(piece: Piece, position: Point)(field: Field): Boolean =
      piece.map(_ |+| position).exists(p => field.lift(p._2).flatMap(_.lift(p._1)).getOrElse(true))

  }

}
