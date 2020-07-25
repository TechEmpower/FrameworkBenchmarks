import basolato/view
import head

proc applicationView*(this:View, title:string, body:string):string = tmpli html"""
<!DOCTYPE html>
<html lang="en">
  $(headView(title))
<body>
  $body
</body>
</html>
"""
