require 'trenni'
require 'erb'
require_relative 'pg_db'
require "prettyprint"
require 'rack'
require 'benchmark'
require 'benchmark/ips'
#Rack::Utils.escape_html
template_prefix='<!DOCTYPE html>
<html>
<head>
  <title>Fortune</title>
</head>
<body>
  <table>
    <tr>
      <th>id</th>
      <th>message</th>
    </tr>'
    template_postfix='</table>
    </body
  </html>'

trenni_buffer=Trenni::Buffer(template_prefix << '<?r self.each do |item| ?> <tr><td> #{item[:id]} </td> <td>#{Rack::Utils.escape_html(item[:message])}</td></tr><?r end ?>' << template_postfix)
trenni_template=Trenni::Template.new(trenni_buffer)

erb_template =  ERB.new(template_prefix << '<% @fortunes.each do |item| %> <tr><td> <%= item[:id] %> </td> <td> <%= Rack::Utils.escape_html(item[:message]) %> </td></tr><% end %>' << template_postfix)

# Produce result.





db=PgDb.new( "postgresql://postgres/hello_world?user=benchmarkdbuser&password=benchmarkdbpass")
@fortunes = db.get_fortunes

iterations=10_000
Benchmark.ips do |x|
	x.report("trenni") do |i|
		iterations.times do
      trenni_template.to_string(@fortunes)
		end
	end
	#  x.report("erb") do |i|
	#  	iterations.times do
  #      erb_template.run()
	#  	end
	#  end
	x.report("String interpolate") do |i|
		iterations.times do
      buffer = String.new
		  buffer << template_prefix
      @fortunes.each do | item|
        buffer <<  "<tr><td> #{item[:id]} </td> <td>#{Rack::Utils.escape_html(item[:message])}</td></tr>"
      end
      buffer << template_postfix

	  end
  end
	x.compare!
end
#pp fortunes

#pp trenni_template.to_string(fortunes)
#pp erb_template.run()


# html = String.new(<<~'HTML')
# <!DOCTYPE html>
# <html>
# <head>
#   <title>Fortunes</title>
# </head>

# <body>

# <table>
# <tr>
#   <th>id</th>
#   <th>message</th>
# </tr>
# HTML

# fortunes.each do |fortune|
# html << <<~"HTML"
# <tr>
#   <td>#{fortune.id}</td>
#   <td>#{Rack::Utils.escape_html(fortune.message)}</td>
# </tr>
# HTML
# end

# html << <<~'HTML'
# </table>

# </body>
# </html>
# HTML
# end


# <table>
# <tr>
#   <th>id</th>
#   <th>message</th>
# </tr>
# <% @fortunes.each do |fortune| %>
# <tr>
#   <td><%= fortune.id %></td>
#   <td><%= fortune.message %></td>
# </tr>
# <% end %>
# </table>

