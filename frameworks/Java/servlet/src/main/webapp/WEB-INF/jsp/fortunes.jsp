<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ page import="hello.*,
                 java.util.*" %><%@ page session="false" %><%

List<Fortune> fortunes = (List)request.getAttribute("fortunes");

%>
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
<td><c:out value="${o.getId()}" /></td>
<td><c:out value="${o.getMessage()}" /></td>
</tr>
</c:forEach>
</table></body>
</html>
