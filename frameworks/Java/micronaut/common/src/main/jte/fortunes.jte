@param java.util.List<benchmark.model.Fortune> fortunes
<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>@for(benchmark.model.Fortune fortune : fortunes)
    <tr><td>${fortune.id()}</td><td>${fortune.message()}</td></tr>@endfor
</table></body></html>