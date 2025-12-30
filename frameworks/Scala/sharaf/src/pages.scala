package ba.sake.sharaf.benchmark

import scalatags.Text.all.*
import ba.sake.hepek.html.HtmlPage

class FortunesPage(rows: Seq[FortuneRow]) extends HtmlPage {
  override def pageSettings = super.pageSettings.withTitle("Fortunes")

  override def bodyContent = table(
    tr(
      th("id"),
      th("message")
    ),
    rows.map { row =>
      tr(
        td(row.id),
        td(row.message)
      )
    }
  )
}
