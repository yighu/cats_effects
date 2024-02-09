import cats.effect.{ExitCode, Fiber, IO, IOApp}

import scala.concurrent.duration.DurationInt

object AsynchrounsIOs extends IOApp{

  val  meaningOfLife = IO(42)
  val favLang: IO[String] = IO("Scala")
  def createFiber: Fiber[IO, Throwable, String] = ???

  def sameThread() = for {
    _ <- meaningOfLife.debug()
    _ <- favLang.debug()
  }yield ()

  val aFiber = meaningOfLife.debug().start

  def differentThreads() = for {
    _ <- aFiber.debug()
    _ <- favLang.debug()
  }yield ()

  def runOnAnotherThread[A](io: IO[A]) = for {
    fib <- io.start //fib = Fiber
    result <- fib.join
  }yield result

  /*
   *1-success(IO(value))
   *2-errored(ex)
   *3-cancelled
   */
  def thrownOnanotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  }yield result

  def testCancel() = {
    val task = IO("starting").debug() *> IO.sleep(1.second) *> IO("done").debug()
    for {
      fib <- task.start
      _ <- IO.sleep(500.millis) *> IO("canceling").debug()
      _ <- fib.cancel
      result <- fib.join
    }yield result
  }
  override def run(args: List[String]): IO[ExitCode] =
    testCancel().debug().as(ExitCode.Success)
  //  thrownOnanotherThread().debug().as(ExitCode.Success)

  // runOnAnotherThread(meaningOfLife).debug().as(ExitCode.Success)
  //differentThreads().as(ExitCode.Success)
}
