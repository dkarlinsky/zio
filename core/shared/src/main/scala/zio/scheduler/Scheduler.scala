/*
 * Copyright 2017-2020 John A. De Goes and the ZIO Contributors
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

package zio.scheduler

import zio.{ UIO, ZLayer }
import zio.internal.{ Scheduler => IScheduler }

object Scheduler extends PlatformSpecific {
  trait Service extends Serializable {
    val scheduler: UIO[IScheduler]
  }
  val live: ZLayer.NoDeps[Nothing, Scheduler] = ZLayer.succeed {
    new Service {
      val scheduler: UIO[IScheduler] =
        UIO.succeed(globalScheduler)
    }
  }
}
