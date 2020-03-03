<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
	<tr><th>id</th><th>message</th></tr>

	@foreach($rows as $row)
		<tr><td>{{$row->id}}</td><td>{{$row->message}}</td></tr>
	@endforeach
</table>
</body>
</html>