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
  <#list data as fortune>
    <tr>
      <td>${fortune.id?html}</td>
      <td>${fortune.message?html}</td>
    </tr>
  </#list>
</table>
</body>
</html>