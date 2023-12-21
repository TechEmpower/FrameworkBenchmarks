import basolato/view
import ./head_view


proc applicationView*(title:string, body:Component):Component =
  tmpli html"""
    <!DOCTYPE html>
    <html lang="en">
      $(headView(title))
    <body>
      $(body)
    </body>
    </html>
  """
