#! stdtmpl | standard
#from xmltree import escape
#import model
#proc fortunes_tmpl*(fortunes: openArray[TFortune]): string =
#  result = ""
<!DOCTYPE html>
<html>
  <head><title>Fortunes</title></head>
<body>
  <table>
    <tr><th>id</th><th>message</th></tr>
  #for fortune in items(fortunes):
    <tr><td>${fortune.id}</td><td>${escape(fortune.message)}</td></tr>
  #end for
  </table>
</body>
</html>
