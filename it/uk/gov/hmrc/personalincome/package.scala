/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc

import java.io.InputStream

import _root_.play.api.libs.json.{JsValue, Json}

import scala.io.Source

package object personalincome {

  def resourceAsString(resourcePath: String): Option[String] =
    withResourceStream(resourcePath) { is =>
      Source.fromInputStream(is).mkString
    }

  def getResourceAsString(resourcePath: String): String =
    resourceAsString(resourcePath).getOrElse(throw new RuntimeException(s"Could not find resource $resourcePath"))

  def resourceAsJsValue(resourcePath: String): Option[JsValue] =
    withResourceStream(resourcePath) { is =>
      Json.parse(is)
    }

  def getResourceAsJsValue(resourcePath: String): JsValue =
    resourceAsJsValue(resourcePath).getOrElse(throw new RuntimeException(s"Could not find resource $resourcePath"))

  private def withResourceStream[A](resourcePath: String)(f: (InputStream => A)): Option[A] =
    Option(getClass.getResourceAsStream(resourcePath)) map { is =>
      try {
        f(is)
      } finally {
        is.close()
      }
    }

}
