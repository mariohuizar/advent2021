import mill._, scalalib._

object advent2021 extends ScalaModule {
  def scalaVersion = "2.13.7"
  val zioVersion = "1.0.12"
  def ivyDeps = Agg(
    ivy"dev.zio::zio:${zioVersion}"
  )
}
