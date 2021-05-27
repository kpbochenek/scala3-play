name := "scala3"
version := "0.1.0"
scalaVersion := "3.0.0"

val zioVersion = "1.0.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.1.1",
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

