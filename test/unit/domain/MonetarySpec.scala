package unit.domain

import domain.Example
import org.scalatest.Matchers
import play.api.libs.json.Json
import uk.gov.hmrc.play.test.UnitSpec

class MonetarySpec extends UnitSpec with Matchers {

  def moneyJson(m: String) = s"""{"money":$m}"""
  
  val m: Map[Double, String] = Map(
    100D -> "100.00",
    100.4D -> "100.40",
    100.9D -> "100.90",
    200.24D -> "200.24",
    200.44D -> "200.44",
    200.49D -> "200.49",
    200.99D -> "200.99",
    200.49D -> "200.49",
    300.345D -> "300.34",
    300.346D -> "300.34",
    300.389D -> "300.38",
    300.999D -> "300.99",
    300.991D -> "300.99"
  )

  "monetary" should {
    m.foreach {
      case (d, s) => s"be translated from value $d to ${s}" in {
        val jsonGenerated = Json.toJson(Example("test",d)).toString()
        val jsonExpected =  s"""{"text":"test","number":$s}"""
        jsonExpected shouldBe jsonGenerated
      }
    }
  }
}