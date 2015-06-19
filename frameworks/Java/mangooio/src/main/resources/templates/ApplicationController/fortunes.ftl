<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
<tr><th>id</th><th>message</th></tr>
<#if fortunes?has_content>
<#list fortunes as fortune>
<tr><td>${fortune.id}</td><td>${fortune.message}</td></tr>
</#list>
</#if>
</table>
</body>
</html>