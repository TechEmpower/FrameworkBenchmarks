<!DOCTYPE html>
<html>
<head>
  <title>Fortunes</title>
</head>
<body>
  <table>
    <tr><th>id</th><th>message</th></tr>
    $fortunes:{ fortune | <tr><td>$fortune.id$</td><td>$fortune.message$</td></tr>}$
  </table>
</body>
</html>