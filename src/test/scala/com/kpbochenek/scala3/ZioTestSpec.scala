package com.kpbochenek.scala3

import zio._
import zio.console._
import zio.clock._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import zio.duration._

object ZioTestSpec extends DefaultRunnableSpec {

  case class Bread(typ: String, size: Int)

  class BreadService {
    println(s"Creating BreadService :+1: ")
    def makeBread(typ: String): Bread = Bread(typ, 20)
  }

  class RefreshService(map: Ref[Map[String, Int]]) {
    println(s"Creating RefreshService :-1: ")

    def perform(): ZIO[Any, Nothing, Unit] = {
      println(s"REFRESH")
      ZIO.unit
    }
  }

  val breadLayer: ZLayer[Any, Nothing, Has[BreadService]] = ZLayer.fromEffect { ZIO.succeed(new BreadService()) }

  val managedQueue: Managed[Nothing, Queue[Int]] = Managed.make(Queue.unbounded[Int])(_.shutdown)

  def loopJob(map: Ref[Map[String, Int]]): ZIO[Clock, Nothing, Unit] =
    (increaseBy1(map)).repeat(Schedule.fixed(1.second)).unit

  private def increaseBy1(map: Ref[Map[String, Int]]): ZIO[Any, Nothing, Unit] =
    map.update(m => m.updated("X", 1)) *> ZIO.succeed(println("Tick"))

  val refreshLayer: ZLayer[Clock, Nothing, Has[RefreshService]] = ZLayer.fromEffect { for {
      map <- Ref.make(Map.empty[String, Int])
      f <- loopJob(map).forkDaemon
      st <- f.status
      _ = println(s"STATUS => ${st}")
    } yield new RefreshService(map)
  }

  override def spec = suite("ZioTestSpec")(
    testM("example1") {
      for {
        breadService <- ZIO.service[BreadService]
        value <- ZIO.succeed(1)
      } yield assert(breadService.makeBread("A"))(equalTo(Bread("A", 20)))
    }.provideLayer(breadLayer),
    testM("example2") {
      for {
        refresh <- ZIO.service[RefreshService]
        value <- ZIO.succeed(3)
        _ <- refresh.perform()
        _ <- TestClock.adjust(5.seconds)
      } yield assert(value)(equalTo(3))

    },
    testM("example3") {
      for {
        breadService <- ZIO.service[BreadService]
      } yield assert(1)(equalTo(1))
    }.provideLayer(breadLayer),
    testM("example3") {
      for {
        value <- ZIO.succeed(5)
        value2 = {Thread.sleep(4000); 13 }
      } yield assert(value + value2)(equalTo(18))
    }
  ).provideLayer(refreshLayer)
}
