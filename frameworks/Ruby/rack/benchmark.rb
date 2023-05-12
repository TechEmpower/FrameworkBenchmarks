#!/usr/bin/env ruby

require 'trenni'
require 'erb'
require_relative 'pg_db'
require "prettyprint"
require 'rack'
require 'benchmark'
require 'benchmark/ips'
require 'pry'
require 'oj'
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
#pp trenni_buffer

trenni_template=Trenni::Template.new(trenni_buffer)

erb_template =  ERB.new('' << template_prefix << '<% @fortunes.each do |item| %> <tr><td> <%= item[:id] %> </td> <td> <%= Rack::Utils.escape_html(item[:message]) %> </td></tr><% end %>' << template_postfix)
#pp erb_template
#exit
# Produce result.






db=PgDb.new()


@fortunes = db.get_fortunes
iterations =50

# pp Oj.dump(db.connection["SELECT id, randomNumber FROM World WHERE id = ?", db.random_id].first)
# pp Oj.dump(r = db.connection["SELECT id, randomNumber FROM World WHERE id = ?", db.random_id].async.first)
# pp Oj.dump(r.to_hash)

#binding.pry
Benchmark.ips do |x|
	x.report("sync") do |i|
    results = []

		iterations.times do
      results << db.connection["SELECT id, randomNumber FROM World WHERE id = ?", db.random_id].first
		end
   # pp results
	end

	x.report("async") do |i|

    promises =[]
    results = []

		iterations.times do
      promises << db.connection["SELECT id, randomNumber FROM World WHERE id = ?", db.random_id].async.first
	  end
    promises.each do | p|
      results << p.to_hash
    end
  end
	x.compare!
end

#pp results
#pp results2


# iterations=10_000
# Benchmark.ips do |x|
# 	x.report("trenni") do |i|
# 		iterations.times do
#       trenni_template.to_string(@fortunes)
# 		end
# 	end
# 	#  x.report("erb") do |i|
# 	#  	iterations.times do
#   #      erb_template.run()
# 	#  	end
# 	#  end
# 	x.report("String interpolate") do |i|
# 		iterations.times do
#       buffer = String.new
# 		  buffer << template_prefix
#       @fortunes.each do | item|
#         buffer <<  "<tr><td> #{item[:id]} </td> <td>#{Rack::Utils.escape_html(item[:message])}</td></tr>"
#       end
#       buffer << template_postfix

# 	  end
#   end
# 	x.compare!
# end
#





