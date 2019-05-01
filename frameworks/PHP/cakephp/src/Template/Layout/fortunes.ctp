<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
	<tr>
		<th>id</th>
		<th>message</th>
	</tr>
	<?= $this->fetch('content') ?>
</table>
</body>
</html>