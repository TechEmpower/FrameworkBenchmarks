<%@ page import="com.techempower.gemini.*, com.techempower.gemini.filters.*" %><%

Integer errorType = (Integer)request.getAttribute("ErrorType");

int type = AccessFilter.ERROR_UNKNOWN;
if (errorType != null)
{
  type = errorType.intValue();
}

%><html>
<head>
<title>Access Check Error</title>
<style>
body
{
  background-color: white;
  color: black;
  font-family: Tahoma, Verdana, Arial, Helvetica;
  font-size: 12px;
  margin: 20px 20px 20px 20px;
}

h2
{
  font-size: 16px;
  border-bottom: 2px solid #405060;
}

.error
{
  border-left: 8px solid #B00810;
  padding-left: 6px;
  padding-top: 3px;
  padding-bottom: 4px;
  text-align: justify;
}

.narrow
{
  width: 450px;
}

.footer
{
  font-size: 10px;
  border-top: 1px dotted #DFDFDF;
  font-style: italic;
}

a, a:visited
{
  color: #800000;
}

</style>
</head>
<body>

<div class="narrow">
<h2>Access Check Error</h2>
<p class="error"><% if (type == AccessFilter.ERROR_NO_ACCESS) { 
%>
You have requested content to which you do not have "read" privileges.
Please contact a system administrator if you believe you are seeing this
message in error.  You may return to the <a href="/">home page</a>.<%
} else {
%>The system was not able to determine your authorization level.  You cannot view the
requested content at this time.  Please contact a system administrator for
assistance.  You may return to the <a href="/">home page</a>.<% } %>
</p>

<p class="footer">
Gemini (v<%= GeminiConstants.GEMINI_VERSION %>) Access Control Filter
</p>
</div>

</body>
</html>
