package component

import component.steps.Env
import cucumber.api.CucumberOptions
import cucumber.api.junit.Cucumber
import org.junit.{BeforeClass, AfterClass}
import org.junit.runner.RunWith

@RunWith(classOf[Cucumber])
@CucumberOptions(
  features = Array("features"),
  glue = Array("component/steps"),
  format = Array("pretty",
    "html:target/component-reports/cucumber",
    "json:target/component-reports/cucumber.json"),
  tags = Array("~@wip")
)
class FeatureSuite

object FeatureSuite {
  @BeforeClass
  def beforeCukesRun() = Env.startServer

  @AfterClass
  def afterCukesRun() = Env.shutdown()
}