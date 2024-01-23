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
    <g:each in="${fortunes}">
        <tr>
            <td>${it.id}</td>
            <td>${it.message.encodeAsHTML()}</td>
        </tr>
    </g:each>
</table>
</body>
</html>