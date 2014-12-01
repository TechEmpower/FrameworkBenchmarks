r = Nginx::Request.new

# https://github.com/mattn/mruby-mysql/blob/master/example/example.rb
db = Userdata.new("my_#{Process.pid}").db

fortunes = []
db.execute("select * from Fortune") do |row, fields|
  fortunes << Hash[fields.zip(row)]
end
fortunes << { "id" => 0, "message" => "Additional fortune added at request time." }

fortunes = fortunes.sort_by { |x| x.message }

#TODO: use "erb" to render template
Nginx.rputs JSON::stringify(fortunes)
