@page
@controller FortunesController
@model {FortunesModel}
<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body><table><tr><th>id</th><th>message</th></tr>
@foreach (var item in @model.Fortune)
{<tr><td>@item.id</td><td>@(System.Web.HttpUtility.HtmlEncode(item.message))</td></tr>}
</table></body>
</html>