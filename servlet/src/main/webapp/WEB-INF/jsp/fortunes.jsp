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
<% for (Fortune fortune : fortunes) { %><tr>
<td><%= fortune.getId() %></td>
<td><%= Common.render(fortune.getMessage()) %></td>
</tr>
<% } %>
</table></body>
</html>