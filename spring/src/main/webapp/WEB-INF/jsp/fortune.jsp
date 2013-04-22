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
<c:forEach var="o" items="${fortunes}">
<tr>
<td>${o.id}</td>
<td>${o.message}</td>
</tr>
</c:if>
</table></body>
</html>
