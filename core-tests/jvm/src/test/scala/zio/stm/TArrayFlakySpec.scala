package zio.stm

import zio.{ TestRuntime, ZIO }
import scala.util.control.Breaks._

class TArrayFlakySpec(implicit ee: org.specs2.concurrent.ExecutionEnv) extends TestRuntime {

  def is = "TArrayFlaky".title ^ s2"""
     not reproduce TArray.fold flakiness $reproduceFlakiness
  """

  private val N = 1000
  private def makeTArray[T](n: Int)(a: T) =
    ZIO.sequence(List.fill(n)(TRef.makeCommit(a))).map(refs => TArray(refs.toArray))

  def reproduceFlakiness = {
    val iterations = 100
    val parallel   = 50
    val total      = iterations * parallel
    var badResult  = Option.empty[String]
    breakable {
      for (i <- 1 to iterations) {
        unsafeRun(
          ZIO.sequencePar(Seq.fill(parallel)(foldAtomic))
        ).zipWithIndex.foreach {
          case (result, j) =>
            if (result != 0 && result != N) {
              val iteration = i * parallel + j
              badResult = Some(s"Result[$iteration/$total] $result")
              break()
            }
        }
      }
    }
    badResult aka "badResult" must beNone
  }

  def foldAtomic =
    for {
      tArray    <- makeTArray(N)(0)
      sum1Fiber <- tArray.fold(0)(_ + _).commit.fork
      _         <- STM.foreach(0 until N)(i => tArray.array(i).update(_ + 1)).commit
      sum1      <- sum1Fiber.join
    } yield sum1
}
