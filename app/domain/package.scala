import play.api.libs.json._

package object domain {

  implicit val doubleWrite = new Writes[Double] {
    def writes(value: Double): JsValue = JsNumber(
      BigDecimal(value).setScale(2, BigDecimal.RoundingMode.FLOOR)
    )
  }

  implicit val exampleFmt = Json.format[Example]

}
