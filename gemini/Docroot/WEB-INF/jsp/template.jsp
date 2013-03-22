<%@ page extends="hello.GhJsp" %><%@ include file="/WEB-INF/jsp/include-variables.jsp" %><%

  // -----------------------------------------------------------------
  // Template page
  //
  // author: mhixson
  //
  // This is a template to use when creating new JSPs.  All JSPs 
  // should have the same basic structure as this template.  
  // Embedded <script> tags, if needed, should be placed after 
  // "include-page-end".
  // -----------------------------------------------------------------
  
  vars.title = "GeminiHello Template";
  vars.stylesheets.add("foo.css");
  vars.scripts.add("bar.js");
  
%><%@ include file="/WEB-INF/jsp/include-page-start.jsp" %>

<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis sit 
amet purus arcu, non luctus ligula. Nullam pharetra euismod vestibulum. 
Vivamus convallis ullamcorper tellus mattis lobortis. Duis at purus 
ullamcorper ipsum posuere bibendum. Quisque ut ligula eu velit varius 
pellentesque et sit amet libero. Suspendisse varius ante dui. Donec 
vulputate dictum dui, vitae varius tortor fermentum adipiscing. Donec 
auctor varius ullamcorper. Aenean nunc nulla, laoreet nec auctor a, 
tempor eget risus. Suspendisse interdum, est faucibus pharetra ultrices, 
tortor libero pulvinar sem, a imperdiet lacus justo at dui. Etiam at 
magna leo, ac laoreet lectus. Pellentesque fringilla mauris nec enim 
porttitor in adipiscing lacus semper.</p>

<%@ include file="/WEB-INF/jsp/include-page-end.jsp" %>