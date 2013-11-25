<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
<tr><th>id</th><th>message</th></tr>
<g:each var="fortune" in="${fortunes}"><tr><td>${fortune.id}</td><td>${fortune.message}</td></tr></g:each>
</table>
</body>
</html>