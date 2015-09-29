<%@ include file="/WEB-INF/tags/taglibs.jsp" %>

<s:layout-definition>

    <!DOCTYPE html>
    <html>

    <script type="text/javascript">
        var cssPath = '${actionBean.cssPath}';
        var contextPath = '${actionBean.contextPath}';
    </script>

    <head>
        <%@page pageEncoding="UTF-8"%>
        <s:layout-component name="head">
        </s:layout-component>
    </head>

    <body>
    <s:layout-component name="body">
        page content goes here
    </s:layout-component>
    </body>
    </html>

</s:layout-definition>

