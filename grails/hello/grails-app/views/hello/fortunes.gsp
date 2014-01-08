<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
<tr><th>id</th><th>message</th></tr>
<g:each var="fortune" in="${fortunes}"><tr><td><g:if test="${fortune.id > 0}">${fortune.id}</g:if><g:else>0</g:else></td><td>${fortune.message}</td></tr></g:each>
</table>
</body>
</html>