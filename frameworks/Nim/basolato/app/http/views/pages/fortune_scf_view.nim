#? stdtmpl(toString="toString") | standard
#import std/asyncdispatch
#import basolato/view
#import ../../../models/fortune
#proc fortuneScfView*(rows:seq[Fortune]):Future[Component] {.async.} =
# result = Component.new()
<!DOCTYPE html>
<html>

<head>
  <title>Fortunes</title>
</head>

<body>
  <table>
    <tr>
      <th>id</th>
      <th>message</th>
    </tr>
    #for row in rows:
      <tr>
        <td>${row.id}</td>
        <td>${row.message}</td>
      </tr>
    #end for
  </table>
</body>

</html>
