import json
import basolato/view

proc impl(title:string, data:seq[JsonNode]):string = tmpli html"""
<!DOCTYPE html>
<html>

<head>
  <title>$title</title>
</head>

<body>
  <table>
    <tr>
      <th>id</th>
      <th>message</th>
    </tr>
    $for row in data{
      <tr>
        <td>$(row["id"].get)</td>
        <td>$(row["message"].get)</td>
      </tr>
    }
  </table>
</body>

</html>
"""

proc fortuneView*(this:View, data=newSeq[JsonNode]()):string =
  let title = "Fortunes"
  return impl(title, data)
