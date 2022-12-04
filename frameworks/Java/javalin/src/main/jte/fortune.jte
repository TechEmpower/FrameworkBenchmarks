@import benchmark.model.Fortune
@import java.util.List

@param List<Fortune> list

<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
    <tr>
        <th>id</th>
        <th>message</th>
    </tr>
    @for(var fortune : list)
        <tr>
            <td>${fortune.getId()}</td>
            <td>${fortune.getMessage()}</td>
        </tr>
    @endfor
</table>
</body>
</html>