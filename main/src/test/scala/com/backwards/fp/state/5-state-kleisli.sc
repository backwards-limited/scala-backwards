import cats.data._
import cats.effect.IO
import cats.implicits._

// Reader
final case class Config(dbName: String)

final case class Result(value: String)

val r = Reader[Config, Result](c =>
  Result(s"Result from database ${c.dbName}")
)

r

r.run(Config("my-db"))

// Reader best used with another effect such as IO
type Id = String

def someIO: Kleisli[IO, Id, Result] =
  Kleisli(id =>
    IO(Result(s"($id) Did some IO"))
  )

def moreIO: Kleisli[IO, Id, Result] =
  Kleisli(id =>
    IO(Result(s"($id) Did some MORE IO"))
  )

val k = for {
  x <- someIO
  y <- moreIO
} yield List(x, y)

k.run("my-id").unsafeRunSync

// Compose functions
val inc: Int => Int =
  _ + 1

val times2: Int => Int =
  _ * 2

val f = inc andThen times2

f(10)

// Compose Monadic functions with Keisli
val incEff: Int => Option[Int] =
  i => (i + 1).some

val times2Eff: Int => Option[Int] =
  i => (i * 2).some

val fEff = Kleisli(incEff) andThen times2Eff

fEff.run(10)
