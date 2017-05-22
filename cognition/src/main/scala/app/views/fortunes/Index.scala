package views.fortunes

import scala.collection.mutable.ArrayBuffer
import models._
import scalatags.all._
import scalatags.Tags2

object Index {

  private[this] final val doctype = "<!DOCTYPE html>"

  def apply(fortunes: ArrayBuffer[Fortune]) = doctype +
    html(
      head(
        Tags2.title("Fortunes")
      ),
      body(
        table(
          tr(
            th("id"),
            th("message")
          ),
          fortunes.map { fortune =>
            tr(
              td(fortune.id.toString),
              td(fortune.message)
            )
          }
        )
      )
    )

}
