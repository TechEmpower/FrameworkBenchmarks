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
  <#list fortune as data>
    <tr>
      <td>${fortune.id}</td>
      <td>${fortune.message}</td>
    </tr>
  </#list>
</table>
</body>
</html>