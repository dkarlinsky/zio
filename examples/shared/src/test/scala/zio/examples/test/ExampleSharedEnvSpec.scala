package zio.examples.test

import zio.test._
import zio.{ UIO, ZIO }

object ExampleSharedEnvSpec extends DefaultRunnableSpec {

  def spec = suite("some suite")(
    test("failing test") {
      assert(1)(Assertion.equalTo(2))
    },
    test("passing test") {
      assert(1)(Assertion.equalTo(1))
    },
    testM("test requires env") {
      assertM(ZIO.environment[Int])(Assertion.equalTo(42))
    }
  ).provideManagedShared(UIO(43).toManaged_)
}
