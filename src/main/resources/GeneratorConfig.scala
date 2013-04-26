/*
 * Copyright 2012 Kazuhiro Sera
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */
package squeryl2scalikejdbc

case class GeneratorConfig(
  srcDir: String = "src/main/scala",
  testDir: String = "src/test/scala",
  packageName: String = "models",
  useJoda: Boolean = true,
  template: GeneratorTemplate = GeneratorTemplate.interpolation,
  testTemplate: Option[GeneratorTestTemplate] = None,
  lineBreak: LineBreak = LineBreak("\n"),
  encoding: String = "UTF-8"
)

object GeneratorTemplate {

  case object basic extends GeneratorTemplate("basic")
  case object namedParameters extends GeneratorTemplate("namedParameters")
  case object executable extends GeneratorTemplate("executable")
  case object interpolation extends GeneratorTemplate("interpolation")

}

sealed abstract class GeneratorTemplate(name: String)

object GeneratorTestTemplate {
  case object ScalaTestFlatSpec extends GeneratorTestTemplate("ScalaTestFlatSpec")
  case object specs2unit extends GeneratorTestTemplate("specs2unit")
  case object specs2acceptance extends GeneratorTestTemplate("specs2acceptance")
}

sealed abstract class GeneratorTestTemplate(name: String)

object LineBreak {
  def value(name: String) = name match {
    case "CR" => "\r"
    case "LF" => "\n"
    case "CRLF" => "\r\n"
    case _ => "\n"
  }
}

case class LineBreak(name: String) {
  def value = LineBreak.value(name)
}

