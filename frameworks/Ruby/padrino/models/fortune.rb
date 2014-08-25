class Fortune
  include DataMapper::Resource

  storage_names[:default] = 'Fortune'

  # property <name>, <type>
  property :id, Serial
  property :message, String
end
