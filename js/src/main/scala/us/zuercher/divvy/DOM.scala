package us.zuercher.divvy

import org.scalajs.jquery.jQuery
import scala.scalajs.js.annotation._

object DOM {
  def main(Args: Array[String]) {
    jQuery("body").append("<p>it worked</p>")
  }
}
