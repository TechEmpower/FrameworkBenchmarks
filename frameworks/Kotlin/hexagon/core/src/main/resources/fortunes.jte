@import java.util.*
@import com.hexagonkt.model.Fortune

@param List<Fortune> fortunes

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
  @for(var fortune : fortunes)
  <tr>
    <td>${fortune.getId()}</td>
    <td>${fortune.getMessage()}</td>
  </tr>
  @endfor
</table>
</body>
</html>
