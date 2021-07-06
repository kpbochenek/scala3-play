package com.kpbochenek.scala3

import zio._
import zio.clock._
import zio.duration._

object ZioMain extends App {

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = (for {
    _ <- ZIO.succeed(println("ZIO main yay!"))
    value <- narrow
    _ = println(value)
    _ <- (ZIO.succeed(println("ITER!"))).repeat(Schedule.fixed(1.second)).fork
//    clock <- ZIO.environment[Clock]
    _ <- sleep(10.second)
    valueX <- narrowThrow *> narrowThrow
    value2 <- narrowErr
    _ = println(value2)
  } yield 3).absorb.catchSome { case e => ZIO.succeed(5) }.map(v => println(s"RESULT => ${v}")).exitCode


  def narrow: ZIO[Any, CustomError, String] = ZIO.succeed("NICE!")
  def narrowErr: ZIO[Any, CustomError, String] = ZIO.fail(Error2(6))
  def narrowThrow: ZIO[Any, CustomError, String] = ZIO.succeed { println("eff"); throw new IllegalStateException("uh oh")}
}

sealed trait CustomError extends Exception
case class Error1(msg: String) extends CustomError
case class Error2(value: Int) extends CustomError