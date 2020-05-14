package mvars

import cats.effect.concurrent.MVar
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.instances.list._
import cats.syntax.traverse._

object Phonebook extends IOApp {
  type PhoneNumber = String
  type Name = String

  type PhoneBook = Map[Name, PhoneNumber]
  case class PhoneBookState[F[_]: Sync](phoneBook: MVar[F, PhoneBook])

  val newPhoneBookState: IO[PhoneBookState[IO]] = for {
    m <- MVar.empty[IO, PhoneBook]
    _ <- m.put(Map.empty[Name, PhoneNumber])
  } yield PhoneBookState(m)

  def insert(state: PhoneBookState[IO])(name: Name,
                                        phoneNumber: PhoneNumber): IO[Unit] =
    for {
      pb <- state.phoneBook.take
      book = pb + (name -> phoneNumber)
      _ <- state.phoneBook.put(book)
    } yield ()

  def lookup(state: PhoneBookState[IO])(name: Name): IO[Option[PhoneNumber]] =
    for {
      pb <- state.phoneBook.take
      _ <- state.phoneBook.put(pb)
      num = pb.get(name)
    } yield num

  override def run(args: List[String]): IO[ExitCode] =
    for {
      pb <- newPhoneBookState
      _ <- List
        .range(1, 10000)
        .traverse(n => insert(pb)(s"name$n", s"$n-$n$n$n"))
      num1 <- lookup(pb)("name999")
      _ <- IO(println(num1))
      _ <- lookup(pb)("unknown").flatMap(num => IO(println(num)))
    } yield ExitCode.Success
}
