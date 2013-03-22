<%@ page extends="hello.GhJsp" %><%@ include file="/WEB-INF/jsp/include-variables.jsp" %><%

  // -----------------------------------------------------------------
  // Error page
  //
  // author: mhixson
  //
  // The error page is rendered when the controller tier encounters
  // an uncaught exception during its request processing.  The
  // exception is delivered along with an optional description.
  // -----------------------------------------------------------------

  vars.title = "GeminiHello Error";
  
  Throwable exception = (Throwable)context.getDelivery("Exception");
  String description = context.getStringDelivery("Description");
  boolean reveal = context.getBooleanDelivery("RevealStackTrace");
  
  // If reveal is false, the stack trace will be rendered into an HTML
  // comment.  Application authors should consider removing that entirely
  // if they are worried about users being able to find the stack trace.

%><%@ include file="/WEB-INF/jsp/include-page-start.jsp" %>

<h2>Sorry, your request could not be processed due to an error</h2>

<p>
  The request you submitted resulted in an error.  Your request cannot be processed at this time.  Please notify the system administrator.
</p>

<%= reveal ? "<p><pre style=\"color: #508050\">" : "<!--" %>
Exception:
<%= ThrowableHelper.getStackTrace(exception) %>
<%
if (exception instanceof ServletException)
{
  ServletException servletException = (ServletException)exception;
%>

Root cause:
<%= ThrowableHelper.getStackTrace(servletException) %><%
} %>
<%= reveal ? "</pre></p>" : "-->" %>

<p>
  <%= description %>
</p>

<%@ include file="/WEB-INF/jsp/include-page-end.jsp" %>