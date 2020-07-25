import basolato/view

proc headView*(title:string):string = tmpli html"""
<head>
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta charset="UTF-8">
  <title>$title</title>
</head>
"""
