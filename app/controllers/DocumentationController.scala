package controllers

import uk.gov.hmrc.play.microservice.controller.BaseController

trait DocumentationController extends AssetsBuilder with BaseController {
  def documentation(version: String, endpointName: String) = {
    super.at(s"/public/api/documentation/$version", s"${endpointName.replaceAll(" ", "-")}.xml")
  }

  def definition() = {
    super.at(s"/public/api", "definition.json")
  }
}

object DocumentationController extends DocumentationController
