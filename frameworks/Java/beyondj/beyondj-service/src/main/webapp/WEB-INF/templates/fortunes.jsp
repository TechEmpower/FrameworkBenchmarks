<%@ include file="/WEB-INF/tags/taglibs.jsp" %>
<s:layout-render name="/WEB-INF/templates/layout.jsp" title="Fortunes">
    <s:layout-component name="body">

        <table>
            <tr>
                <th>id</th>
                <th>message</th>
            </tr>

            <c:forEach items="${actionBean.fortunes}" var="fortune" varStatus="loopStatus">
                <tr>
                    <td>${fortune.id}</td>
                    <td>${fn:escapeXml(fortune.message)}</td>
                </tr>
            </c:forEach>
        </table>
    </s:layout-component>
</s:layout-render>
