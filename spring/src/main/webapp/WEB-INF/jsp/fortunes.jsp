<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib uri="http://www.springframework.org/tags" prefix="spring"%>
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
<td>${o.getId()}</td>
<td><spring:escapeBody htmlEscape="true">${o.getMessage()}</spring:escapeBody></body></td>
</tr>
</c:forEach>
</table>
</html>
