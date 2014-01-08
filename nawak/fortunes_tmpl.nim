#! stdtmpl | standard
#import lib/escape
#from nawak_app import TFortune
#proc fortunes_tmpl*(fortunes: openArray[TFortune]): string =
#  result = ""
<!DOCTYPE html>
<html>
  <head><title>Fortunes</title></head>
<body>
  <table>
    <tr><th>id</th><th>message</th></tr>
  #for fortune in items(fortunes):
    <tr><td>${fortune.id}</th><th>${escape(fortune.message)}</th></tr>
  #end for
  </table>
</body>
</html>
