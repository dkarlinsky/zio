package zio.test

import zio.test.FailureRenderer.FailureMessage.Message
import zio.test.environment.TestEnvironment
import zio.{UIO, ZIO}

object TestUtils {

  def execute[E, L, S](spec: ZSpec[TestEnvironment, E, L, S]): UIO[ExecutedSpec[E, L, S]] =
    TestExecutor.managed(environment.testEnvironmentManaged).run(spec, ExecutionStrategy.Sequential)

  def exectuedToMap[E, L, S](spec: ExecutedSpec[E, L, S]): UIO[Map[String, Either[String, String]]] = {
    def traverse(spec: ExecutedSpec[E, L, S], path: Vector[L]): UIO[Map[String, Either[String, String]]] = {
      spec.caseValue match {
        case Spec.TestCase(label, resM) => for {
            res      <- resM
            rendered <- renderTestResult(label, res._1)
          } yield Map((path :+ label).mkString("::") -> rendered)
        case Spec.SuiteCase(label, specs, _) =>
          specs.flatMap(ZIO.foreach(_)(traverse(_, path :+ label)).map(_.foldLeft(Map.empty[String, Either[String, String]])(_ ++ _)))
      }
    }
    traverse(spec, Vector.empty)
  }

  def forAllTests[E, L, S](
    execSpec: UIO[ExecutedSpec[E, L, S]]
  )(f: Either[TestFailure[E], TestSuccess[S]] => Boolean): ZIO[Any, Nothing, Boolean] =
    execSpec.flatMap { results =>
      results.forall { case Spec.TestCase(_, test) => test.map(r => f(r._1)); case _ => ZIO.succeed(true) }
    }

  def isIgnored[E, L, S](spec: ZSpec[environment.TestEnvironment, E, L, S]): ZIO[Any, Nothing, Boolean] = {
    val execSpec = execute(spec)
    forAllTests(execSpec) {
      case Right(TestSuccess.Ignored) => true
      case _                          => false
    }
  }

  def isSuccess[E, L, S](spec: ZSpec[environment.TestEnvironment, E, L, S]): ZIO[Any, Nothing, Boolean] = {
    val execSpec = execute(spec)
    forAllTests(execSpec) {
      case Right(TestSuccess.Succeeded(_)) => true
      case _                               => false
    }
  }

  def renderTestResult[E, L, S](label: L, executedResult: ExecutedResult[E, S]): UIO[Either[String, String]] = executedResult match {
    case Left(failure) => renderTestFailure(label, failure).map(Left(_))
    case Right(_) => ZIO.succeed(Right("success"))
  }

  private def renderTestFailure[E, L](label: L, testFailure: TestFailure[E]): UIO[String] = testFailure match  {
    case TestFailure.Assertion(testResult) => FailureRenderer.renderTestFailure(label.toString, testResult).map(renderToString)
    case TestFailure.Runtime(cause) => FailureRenderer.renderCause(cause, 0).map(renderToString)
  }

  private def renderToString(message: Message) =
    message.lines.map {
      _.fragments.map(_.text).fold("")(_ + _)
    }.mkString("\n")
}
