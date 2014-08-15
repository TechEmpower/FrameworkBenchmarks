<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ page session="false" %>
<%
    if(request.getAttribute("statusCode") != null)
        response.setStatus((Integer)request.getAttribute("statusCode"));
    else
        response.setStatus(500);
%>
<html>
<head>
<title>error</title>
</head>
<body>
<h1>error</h1>
${message}
</body>
</html>
