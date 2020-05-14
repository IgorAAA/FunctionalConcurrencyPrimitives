package stms

import java.util.concurrent.TimeUnit

import cats.effect.{Clock, ExitCode, IO, IOApp}
import io.github.timwspence.cats.stm.{STM, TVar}

import scala.concurrent.duration._

object Windows extends IOApp {
  case class Desktop(id: Long)
  case class Window(name: String)

  type Display = Map[Desktop, TVar[Vector[Window]]]

  val clock: Clock[IO] = timer.clock

  def moveWindowSTM(display: Display,
                    window: Window,
                    source: Desktop,
                    dest: Desktop): STM[Unit] =
    for {
      _ <- display(source).modify(v => v.filterNot(_ == window))
      _ <- display(dest).modify(v => window +: v)
    } yield ()

  def moveWindow(display: Display,
                 window: Window,
                 desktop1: Desktop,
                 desktop2: Desktop): IO[Unit] =
    for {
      _ <- IO {
        println(
          s"Thread: ${Thread.currentThread().getName} time: ${clock.realTime(TimeUnit.MILLISECONDS).unsafeRunSync()}"
        )
        Thread.sleep(100)
      }
      _ <- STM.atomically[IO](
        moveWindowSTM(display, window, desktop1, desktop2)
      )
      _ <- IO(
        println(
          s"END: Thread: ${Thread.currentThread().getName} time: ${clock.realTime(TimeUnit.MILLISECONDS).unsafeRunSync()}"
        )
      )
    } yield ()

  def printDisplay(display: Display,
                   desktop1: Desktop,
                   desktop2: Desktop): IO[Unit] =
    for {
      records <- STM.atomically[IO](for {
        vwA <- display(desktop1).get
        vwB <- display(desktop2).get
      } yield Map(desktop1 -> vwA, desktop2 -> vwB))
      _ <- IO(
        println(s"PRINT: Thread: ${Thread.currentThread().getName} Time: ${clock
          .realTime(TimeUnit.MILLISECONDS)
          .unsafeRunSync()}")
      )
      _ <- IO(println(s"Desktop id ${desktop1.id} - ${records(desktop1)}"))
      _ <- IO(println(s"Desktop id ${desktop2.id} - ${records(desktop2)}"))
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    val desktop1 = Desktop(1L)
    val desktop2 = Desktop(2L)
    val windowA = Window("A")
    val windowB = Window("B")
    val initalState =
      Map(desktop1 -> Vector(windowA, windowB), desktop2 -> Vector.empty)

    for {
      d1 <- TVar.of(initalState(desktop1)).commit[IO]
      d2 <- TVar.of(initalState(desktop2)).commit[IO]
      display = Map(desktop1 -> d1, desktop2 -> d2)
      _ <- printDisplay(display, desktop1, desktop2)
      fiber1 <- moveWindow(display, windowA, desktop1, desktop2).start
      fiber2 <- moveWindow(display, windowB, desktop1, desktop2).start
      _ <- timer.sleep(500.millis)
      _ <- printDisplay(display, desktop1, desktop2)
      _ <- fiber1.join
      _ <- fiber2.join
    } yield ExitCode.Success
  }
}
