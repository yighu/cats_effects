import cats.effect.{ExitCode, IO, IOApp}

object InefficientProducerConsumer extends IOApp{
  import cats.effect._
  import cats.effect.std.Console
  import cats.syntax.all._
  import collection.immutable.Queue

  def producer[F[_]: Sync: Console](queueR: Ref[F, Queue[Int]], counter: Int): F[Unit] =
    for {
      _ <- if(counter % 10000 == 0) Console[F].println(s"Produced $counter items") else Sync[F].unit
      _ <- queueR.getAndUpdate(_.enqueue(counter + 1))
      _ <- producer(queueR, counter + 1)
    } yield ()


  def consumer[F[_]: Sync: Console](queueR: Ref[F, Queue[Int]]): F[Unit] =
    for {
      iO <- queueR.modify{ queue =>
        queue.dequeueOption.fold((queue, Option.empty[Int])){case (i,queue) => (queue, Option(i))}
      }
      _ <- if(iO.exists(_ % 10000 == 0)) Console[F].println(s"Consumed ${iO.get} items") else Sync[F].unit
      _ <- consumer(queueR)
    } yield ()


  override def run(args: List[String]): IO[ExitCode] =
    for {
      queueR <- Ref.of[IO, Queue[Int]](Queue.empty[Int])
      res <- (consumer(queueR), producer(queueR, 0))
        .parMapN((_, _) => ExitCode.Success) // Run producer and consumer in parallel until done (likely by user cancelling with CTRL-C)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res

}
