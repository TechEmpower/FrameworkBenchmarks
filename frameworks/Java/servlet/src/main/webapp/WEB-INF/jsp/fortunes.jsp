<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ page session="false"%>
<!DOCTYPE html>
<html>
<head>
<title>Fortunes</title>
</head>
<body>
<table>
<tr><th>id</th><th>message</th></tr>
<c:forEach var="fortune" items="${fortunes}">
<tr><td>${fortune.getId()}</td><td><c:out value="${fortune.getMessage()}" /></td></tr>
</c:forEach>
</table>
</body>
</html>