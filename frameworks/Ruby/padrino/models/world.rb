class World
  include DataMapper::Resource

  storage_names[:default] = 'World'

  # property <name>, <type>
  property :id, Serial
  property :randomNumber, Integer, field: 'randomNumber'
end
