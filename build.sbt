import Dependencies._
import play.PlayImport.PlayKeys.routesImport
import sbt.Keys._
import sbt.Tests.{Group, SubProcess}
import sbt._
import uk.gov.hmrc.DefaultBuildSettings._
import uk.gov.hmrc.NexusPublishing.nexusPublishingSettings
import uk.gov.hmrc.PublishingSettings._
import uk.gov.hmrc.SbtBuildInfo
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._

import scala.util.Properties._

name := "personal-income"
version := envOrElse("PERSONAL_INCOME_TEMP_VERSION", "999-SNAPSHOT")
targetJvm := "jvm-1.8"
resolvers += Resolver.bintrayRepo("hmrc", "releases")

lazy val UnitTest = config("unit") extend Test
lazy val ComponentTest = config("component") extend IntegrationTest

val testConfig = Seq(IntegrationTest, UnitTest, ComponentTest)

lazy val microservice = (project in file("."))
  .enablePlugins(PlayScala)
  .disablePlugins(sbt.plugins.JUnitXmlReportPlugin)
  .configs(testConfig: _*)
  .settings(
    commonSettings,
    staticCompileResourceSettings,
    unitTestSettings,
    componentTestSettings,
    itTestSettings,
    testSettings,
    playSettings,
    playPublishingSettings
  )

lazy val componentTestSettings =
  inConfig(ComponentTest)(Defaults.testSettings) ++
    Seq(
      testOptions in ComponentTest := Seq(Tests.Filter((name: String) => name startsWith "component")),
      unmanagedSourceDirectories in ComponentTest <<= (baseDirectory in ComponentTest)(base => Seq(base / "test/component"))
    )

lazy val unitTestSettings =
  inConfig(UnitTest)(Defaults.testSettings) ++
    Seq(
      testOptions in UnitTest := Seq(Tests.Filter((name: String) => name startsWith "unit")),
      unmanagedSourceDirectories in UnitTest <<= (baseDirectory in UnitTest)(base => Seq(base / "test/unit"))
    )

lazy val itTestSettings =
  inConfig(IntegrationTest)(Defaults.itSettings) ++
    Seq(
      Keys.fork in IntegrationTest := false,
      unmanagedSourceDirectories in IntegrationTest <<= (baseDirectory in IntegrationTest)(base => Seq(base / "test/it")),
      addTestReportOption(IntegrationTest, "int-test-reports"),
      testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
      parallelExecution in IntegrationTest := false
    )

lazy val testSettings =
  inConfig(Test)(Defaults.testSettings) ++
    Seq(
      fork in Test := false,
      unmanagedSourceDirectories in Test <<= (baseDirectory in Test)(base => Seq(base / "test")),
      testGrouping in Test := oneForkedJvmPerTest((definedTests in Test).value),
      parallelExecution in Test := false
    )

lazy val commonSettings: Seq[Setting[_]] = scalaSettings ++
  publishingSettings ++
  defaultSettings() ++
  gitStampSettings ++
  SbtBuildInfo()

lazy val staticCompileResourceSettings =
  unmanagedResourceDirectories in Compile += baseDirectory.value / "resources"

lazy val playSettings: Seq[Setting[_]] = Seq(
  routesImport += "controllers.Binders._"
)

def oneForkedJvmPerTest(tests: Seq[TestDefinition]) = tests map {
  test => new Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
}

lazy val playPublishingSettings: Seq[sbt.Setting[_]] = sbtrelease.ReleasePlugin.releaseSettings ++
  Seq(credentials += SbtCredentials) ++
  publishAllArtefacts ++
  nexusPublishingSettings

libraryDependencies ++= compileDependencies ++ testDependencies
