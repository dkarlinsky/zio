/*
 * Copyright 2019 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.test

import zio.{ Has, UIO, URIO, ZDep, ZIO }
import zio.console.Console

object TestLogger {
  trait Service {
    def logLine(line: String): UIO[Unit]
  }

  def fromConsole: ZDep[Console, Nothing, TestLogger] = 
    ZDep.fromFunction { (console: Console.Service) =>
      Has(new Service {
        def logLine(line: String): UIO[Unit] = console.putStrLn(line)
      })
    }

  def logLine(line: String): URIO[TestLogger, Unit] =
    ZIO.accessM(_.get.logLine(line))
}
