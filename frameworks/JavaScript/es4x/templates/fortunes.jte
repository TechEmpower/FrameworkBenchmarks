@import java.util.*
@param List<Map<String, ?>> fortunes
<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
@for(Map<String, ?> fortune : fortunes)
<tr><td>${(Integer) fortune.get("id")}</td><td>${(String) fortune.get("message")}</td></tr>
@endfor
</table></body></html>
