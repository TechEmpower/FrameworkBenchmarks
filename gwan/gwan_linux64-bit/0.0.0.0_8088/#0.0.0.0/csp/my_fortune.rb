#!/usr/bin/ruby
# Test type 4: Fortunes

require_relative 'db-connect-my'
require_relative 'gwan-helper'

res = DB.query "SELECT id, message FROM Fortune"
rows = res.map { |row| row }
rows << {id: 0, message: 'Additional fortune added at request time.' }
rows.sort! { |a,b| a[:message] <=> b[:message] }

content = '<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
<tr><th>id</th><th>message</th></tr>'
rows.each do |row|
  content += "<tr><td>#{row[:id]}</td><td>#{escape(row[:message])}</td></tr>"
end
content += '</table>
</body>
</html>'

respond content, 'text/html'