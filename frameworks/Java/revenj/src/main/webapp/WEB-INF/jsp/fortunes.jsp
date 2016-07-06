<%@ page import="dsl.FrameworkBench.*,java.util.*,org.apache.commons.lang3.*" session="false" %>
<% List<Fortune> fortunes = (List)request.getAttribute("fortunes"); %>
<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
<tr><th>id</th><th>message</th></tr>
<%  for (int i=0; i<fortunes.size(); i++) {
    Fortune f = fortunes.get(i); %>
<tr><td><%= f.getId() %></td><td><%= StringEscapeUtils.escapeHtml4(f.getMessage()) %></td></tr>
<%  } %>
</table>
</body>
</html>
