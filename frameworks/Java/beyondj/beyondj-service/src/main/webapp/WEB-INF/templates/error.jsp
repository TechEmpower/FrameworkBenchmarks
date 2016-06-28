<%@ include file="/WEB-INF/tags/taglibs.jsp" %>

<s:layout-render name="/WEB-INF/jsp/layout.jsp" title="hurudza error page">
  <s:layout-component name="body">
  		<center/>
    	<div data-role="content" id="errorpage">
			<h3><fmt:message key="errorslabel"/></h3>
			<div id="errorsection">
				<s:errors/>
			</div>
		</div>	
  </s:layout-component>
</s:layout-render>