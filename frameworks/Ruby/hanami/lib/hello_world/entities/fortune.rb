class Fortune < Hanami::Entity
  attributes do
    attribute :id,      Types::Int
    attribute :message, Types::String
  end
end
