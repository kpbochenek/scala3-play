package com.kpbochenek.scala3

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode

object CatsFiberMain extends IOApp {

  val meaningOfLife = IO(42)
  val favLang = IO("Scala")
  
  extension [A] (io: IO[A]) {
    def debug: IO[A] = io.map { value =>
      println(s"[${Thread.currentThread().getName}] $value")
      value
    }
  }

  def sameThread(): IO[String] = for {
    _ <- IO { println(s"[${Thread.currentThread().getName}]Start CATS app") }
    i <- meaningOfLife.debug.start
    s <- favLang.debug.start
    iVal <- i.join
    sVal <- s.join
    iWrap <- iVal.embedNever
    sWrap <- sVal.embedNever
  } yield s"$s -> $sVal -> $sWrap ::: $i -> $iVal -> $iWrap"

  def run(args: List[String]): IO[ExitCode] =
    sameThread().debug.as(ExitCode.Success)

  // def main(args: Array[String]): Unit = {
  //   println("CatsFiber")
  // }
}
