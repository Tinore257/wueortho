// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package drawings

import wueortho.pipeline.{Pipeline, Stage}
import java.nio.file.Paths
import wueortho.pipeline.CoreStep

lazy val mainRuntime = Pipeline.Runtime("core-steps", CoreStep.allImpls)

@main def runPipeline =
  val res = mainRuntime.run(mainRuntime.fromFile(Paths.get("config.json").nn).fold(throw _, identity))
  println(res.runningTime.show)
  println(res.getResult(Stage.Metadata, None).fold(identity, _.show))

@main def showHelp =
  println(mainRuntime.showHelpText)
