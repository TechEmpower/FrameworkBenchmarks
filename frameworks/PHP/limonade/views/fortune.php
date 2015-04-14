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
<?php
foreach($fortunes as $f) {
	echo ("<tr><td>" . $f["id"] . "</td><td>" . $f["message"] . "</td></tr>");
}
?>
</table>
</body>
</html>
